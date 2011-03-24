(in-package :taskgraph)

(defparameter *blessed-task-constructors* (make-hash-table :test #'eq))

(defclass task ()
  ((id :initarg :id
       :reader task-id)
   (epsilon :initform nil
	    :initarg :epsilon
	    :reader task-is-epsilon?)))

(defclass mtask (task)
  ((name :initarg :name
	 :accessor mtask-name)))

(defmethod print-object ((task task) stream)
  (format stream "[~a]" (task-id task)))

(defmethod print-object ((mtask mtask) stream)
  (format stream "[~a: ~a]" (task-id mtask) (mtask-name mtask)))

(defun make-task (id &key (epsilon nil))
  (make-instance 'task :id id :epsilon epsilon))

(defmethod serialize ((mtask mtask))
  (with-slots (id name epsilon)
      mtask
    (list id name :epsilon epsilon)))

(defmethod serialize ((task task))
  (with-slots (id epsilon)
      task
    (list id epsilon)))


(defun make-mtask (id name &key (epsilon nil))
  (make-instance 'mtask :id id :name name :epsilon epsilon))

(defmacro bless-task-constructor (name fobj)
  `(setf (gethash ,name *blessed-task-constructors*) ,fobj))

(bless-task-constructor 'task #'make-task)
(bless-task-constructor 'mtask #'make-mtask)

(defun get-blessed-constructor (fsym)
  (gethash fsym *blessed-task-constructors*))

(defclass graph ()
  ((tasks :initform nil
	  :accessor graph-tasks)
   (dependencies :initform (make-hash-table)
		 :accessor graph-dependencies)
   (dependents :initform (make-hash-table)
	       :accessor graph-dependents)
   (taskmap :initform (make-hash-table)
	    :accessor graph-taskmap)
   (constructor :initarg :constructor)))

(defun serialize-edges (graph)
  (with-slots (tasks dependencies constructor)
      graph
    (let ((edges nil))
      (maphash #'(lambda (task dependencies)
		   (dolist (dependency dependencies)
		     (when dependency
		       (push (list (task-id dependency)
				 (task-id task))
			     edges))))
	       dependencies)
      edges)))

(defun mark-as-epsilon (task)
  (with-slots (epsilon)
      task
    (setf epsilon t)))

(defun mark-as-non-epsilon (task)
  (with-slots (epsilon)
      task
    (setf epsilon nil)))

(defmethod serialize ((graph graph))
  (with-slots (tasks dependencies constructor)
      graph
    (list constructor
	  (mapcar #'serialize tasks)
	  (serialize-edges graph))))

(defun write-graph-to-file (graph filename)
  (with-open-file (stream filename :direction :output :if-exists :overwrite)
    (write (serialize graph) :stream stream)))

(defun make-graph (constructor-name)
  (make-instance 'graph :constructor constructor-name))

(defun add-task (graph &rest rest)
  (with-slots (constructor)
      graph
    (insert-task graph (apply (get-blessed-constructor constructor) rest))))

(defun unserialize-graph (serialized-graph)
  (let* ((task-maker-name (car serialized-graph))
	 (task-maker (get-blessed-constructor task-maker-name))
	 (rv (make-graph task-maker-name)))
    (dolist (serialized-task (cadr serialized-graph))
      (insert-task rv (apply task-maker serialized-task)))
    (dolist (edge (caddr serialized-graph))
      (add-dependency rv (car edge) (cadr edge)))
    rv))

(defun read-graph-from-file (filename)
  (with-open-file (stream filename)
    (unserialize-graph (read stream))))

(defmacro with-graph-in-file (args &body body)
  (let* ((gs (car args))
	 (fn (cadr args)))
    `(let ((,gs (read-graph-from-file ,fn)))
       (unwind-protect
	    (progn
	      ,@body
	      (write-graph-to-file ,gs ,fn))))))
    
(defun graph-task (graph id)
  (gethash id (graph-taskmap graph)))

(defun task-dependencies (graph id)
  (with-slots (dependencies)
      graph
    (let ((task (graph-task graph id)))
      (gethash task dependencies nil))))

(defun task-dependents (graph id)
  (with-slots (dependents)
      graph
    (let ((task (graph-task graph id)))
      (gethash task dependents nil))))

(defun add-dependency (graph dependency-id dependent-id)
  (with-slots (dependents dependencies)
      graph
    (let ((ent (graph-task graph dependent-id))
	  (ency (graph-task graph dependency-id)))
      (pushnew ent (gethash ency dependents nil))
      (pushnew ency (gethash ent dependencies nil)))))

(defun remove-dependency (graph dependency-id dependent-id)
  (with-slots (dependents dependencies)
      graph
    (let ((ent (graph-task graph dependent-id))
	  (ency (graph-task graph dependency-id)))
      (setf (gethash ency dependents)
	    (delete ent (gethash ency dependents)))
      (setf (gethash ent dependencies)
	    (delete ency (gethash ent dependencies))))))

(defun insert-task (graph task)
  (with-slots (tasks taskmap)
      graph
    (multiple-value-bind (value present-p)
	(gethash (task-id task) taskmap)
      (when present-p
	(error (format nil "task collision: ~a (old) vs ~a (new)" value task)))
      (setf (gethash (task-id task) taskmap) task)
      (push task tasks))))

(defun remove-task (graph id)
  (with-slots (tasks taskmap)
      graph
    (multiple-value-bind (value present-p)
	(gethash id taskmap)
      (unless present-p
	(return-from remove-task))
      (setf tasks (remove value tasks))
      (remhash id taskmap)
      (dolist (task tasks)
	(remove-dependency graph value task)
	(remove-dependency graph task value))
      value)))

(let ((u-d-stack nil))
  (defun unmet-dependencies (graph task)
    (push task u-d-stack)
    (with-slots (dependencies)
	graph
      (let ((result (remove-if
		     #'(lambda (dep) (and (task-is-epsilon? dep)
					  (null (find dep u-d-stack))
					  (null (unmet-dependencies graph dep))))
		     (gethash task dependencies))))
	(pop u-d-stack)
	result))))

(defun get-starting-tasks (graph)
  "Get the set of tasks that have no unmet dependencies."
  (with-slots (tasks dependencies)
      graph
    (remove-if #'task-is-epsilon?
	       (remove-if #'(lambda (task) (unmet-dependencies graph task))
			  tasks))))

(defun take-until (element list &key (test #'eql))
  (if (funcall test element (car list))
      nil
      (cons (car list) (take-until element (cdr list) :test test))))

(defun graph-find-cycle (graph &optional (exclude nil))
  (let ((nodes (nset-difference (mapcar #'task-id (graph-tasks graph))
				exclude))
	(cycle nil))
    (labels ((explore (node)
	       (when (find node cycle)
		 (return-from graph-find-cycle (take-until node cycle)))
	       (push node cycle)
	       (dolist (dep (mapcar #'task-id (task-dependencies graph node)))
		 (unless (not (find dep nodes))
		   (explore dep)))
	       (setf nodes (remove node nodes))
	       (pop cycle)))
      (mapcar #'explore nodes)
      nil)))

(defun graph-find-cycles (graph)
  "This function behaves a bit arbitrarily on interlocking cycles, only ever reporting a node as part of zero or one cycles, but determines distinct cycles accurately."
  (do* ((exclude nil)
	(cycle (graph-find-cycle graph exclude) (graph-find-cycle graph exclude))
	(cycles nil))
       ((null cycle) cycles)
    (push cycle cycles)
    (dolist (x cycle)
      (pushnew x exclude))))

(defun graph-has-cycles? (graph)
  (graph-find-cycle graph))

(defun get-graph-problems (graph)
  (let ((rv nil))
    (when (graph-has-cycles? graph)
      (push :has-cycle rv))
    rv))

(defun get-task-ordering (graph)
  "Get a topologically sorted ordering of the tasks in the graph. If there are cycles in the graph, this method will return an incomplete list of tasks, accomplishing as much as can be done without entering the cycles."
  (do ((removed-dependents (make-hash-table))
       (removed-dependencies (make-hash-table))
       (l nil)
       (s (get-starting-tasks graph)))
      ((null s) (reverse l))
    (let ((n (pop s)))
      (unless (task-is-epsilon? n)
	(push n l))
      (dolist (m (set-difference (task-dependents graph (task-id n))
				 (gethash (task-id n) removed-dependents)))
	(push m (gethash (task-id n) removed-dependents))
	(push n (gethash (task-id m) removed-dependencies))
	(unless (set-difference (task-dependencies graph (task-id m))
				(gethash (task-id m) removed-dependencies))
	  (push m s))))))

(defun make-testing-graph ()
  (let ((graph (make-graph #'make-task)))
    (insert-task graph (make-task 'build-foundation))
    (insert-task graph (make-task 'build-walls))
    (insert-task graph (make-task 'build-roof))
    (insert-task graph (make-task 'paint-house-outside))
    (insert-task graph (make-task 'paint-house-inside))
    (insert-task graph (make-task 'furnish-house))
    (insert-task graph (make-task 'move-in))

    (add-dependency graph 'build-foundation 'build-walls)
    (add-dependency graph 'build-walls 'build-roof)
    (add-dependency graph 'build-roof 'paint-house-inside)
    (add-dependency graph 'build-roof 'paint-house-outside)
    (add-dependency graph 'paint-house-inside 'furnish-house)
    (add-dependency graph 'paint-house-outside 'move-in)
    (add-dependency graph 'furnish-house 'move-in)

    (add-dependency graph 'move-in 'build-foundation)

    (insert-task graph (make-task 'meet-neighbours))
    (insert-task graph (make-task 'invite-neighbours-to-dinner))
    (add-dependency graph 'move-in 'invite-neighbours-to-dinner)
    graph))

(defun run-tests (graph)
  (list
   (get-graph-problems graph)
   (get-starting-tasks graph)
   (get-task-ordering graph)))

  