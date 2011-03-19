(asdf:defsystem #:taskgraph
    :depends-on ()
    :components ((:file "package.lisp")
		 (:file "taskgraph.lisp"
			:depends-on ("package"))))