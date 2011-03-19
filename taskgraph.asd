(asdf:defsystem #:taskgraph
    :depends-on ()
    :components ((:file "package")
		 (:file "taskgraph"
			:depends-on ("package"))))
