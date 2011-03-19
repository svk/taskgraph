
(defclass task ()
  ((id :initarg :id
       :reader task-id)))

(defmethod print-object ((task task) stream)
  (format stream "[~a]" (task-id task)))

(defun make-task (id)
  (make-instance 'task :id id))

(defclass graph ()
  ((tasks :initform nil
	  :accessor graph-tasks)
   (dependencies :initform (make-hash-table)
		 :accessor graph-dependencies)
   (dependents :initform (make-hash-table)
	       :accessor graph-dependents)
   (taskmap :initform (make-hash-table)
	    :accessor graph-taskmap)))

(defmethod serialize ((task task))
  (list (task-id task)))

(defmethod serialize ((graph graph))
  (with-slots (tasks dependencies)
      graph
    (list (mapcar #'serialize tasks)
	  (let ((edges nil))
	    (maphash #'(lambda (task dependencies)
			 (dolist (dependency dependencies)
			   (push (list (task-id dependency)
				       (task-id task))
				 edges)))
		     dependencies)
	    edges))))

(defun write-graph-to-file (graph filename)
  (with-open-file (stream filename :direction :output :if-exists :overwrite)
    (write (serialize graph) :stream stream)))

(defun make-graph ()
  (make-instance 'graph))

(defun unserialize-graph (serialized-graph task-maker)
  (let ((rv (make-graph)))
    (dolist (serialized-task (car serialized-graph))
      (format t "~a~%" serialized-task)
      (insert-task rv (apply task-maker serialized-task)))
    (dolist (edge (cadr serialized-graph))
      (add-dependency rv (car edge) (cadr edge)))
    rv))

(defun read-graph-from-file (filename task-maker)
  (with-open-file (stream filename)
    (unserialize-graph (read stream) task-maker)))
    
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
      (format t "adding ~a -> ~a~%" ency ent)
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
      (setf tasks (delete value tasks))
      (remhash id taskmap)
      (dolist (task tasks)
	(remove-dependency graph value task)
	(remove-dependency graph task value))
      value)))

(defun get-starting-tasks (graph)
  "Get the set of tasks that have no unmet dependencies."
  (with-slots (tasks dependencies)
      graph
    (remove-if #'(lambda (task) (gethash task dependencies))
	       tasks)))

(defun graph-has-cycles? (graph)
  (< (length (get-task-ordering graph))
     (length (graph-tasks graph))))

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
      (push n l)
      (dolist (m (set-difference (task-dependents graph (task-id n))
				 (gethash (task-id n) removed-dependents)))
	(push m (gethash (task-id n) removed-dependents))
	(push n (gethash (task-id m) removed-dependencies))
	(unless (set-difference (task-dependencies graph (task-id m))
				(gethash (task-id m) removed-dependencies))
	  (push m s))))))

(defun make-testing-graph ()
  (let ((graph (make-graph)))
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
    (remove-dependency graph 'move-in 'build-foundation)
    (insert-task graph (make-task 'meet-neighbours))
    (insert-task graph (make-task 'invite-neighbours-to-dinner))
    (add-dependency graph 'move-in 'invite-neighbours-to-dinner)
    graph))

(defun run-tests (graph)
  (list
   (get-graph-problems graph)
   (get-starting-tasks graph)
   (get-task-ordering graph)))

    
 