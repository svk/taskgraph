(asdf:defsystem #:cl-gpgi
    :depends-on ()
    :components ((:file "cl-gpgi-package")
		 (:file "run-program"
			:depends-on ("cl-gpgi-package"))
		 (:file "cl-gpgi"
			:depends-on ("cl-gpgi-package"
				     "run-program"))))