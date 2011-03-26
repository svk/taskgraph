(asdf:defsystem #:taskgraph
    :depends-on (#:cl-gpgi)
    :components ((:file "package")
		 (:file "taskgraph"
			:depends-on ("package"))))
