
(defclass task ()
  ((id :initarg :id
       :reader task-id)))

(defmethod print-object ((task task) stream)
  (format stream "[~a]" (task-id task)))

(defun make-task (id)
  (make-instance 'task :id id))

(defstruct graph
  (tasks nil)
  (dependencies (make-hash-table))
  (dependents (make-hash-table))
  (taskmap (make-hash-table :test #'equal)))

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
      (declare (ignore value))
      (when present-p
	(error "task already present"))
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

(defun test-topological-sort ()
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
    (get-task-ordering graph)))

    
 