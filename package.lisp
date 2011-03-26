(defpackage :taskgraph
  (:use :common-lisp)
  (:export #:with-graph-in-file
	   #:with-graph-in-crypto-file

	   #:add-task
	   #:remove-task
	   #:add-dependency
	   #:remove-dependency
	   #:interpose-dependency

	   #:get-starting-tasks
	   #:get-task-ordering
	   #:get-task-ids))
