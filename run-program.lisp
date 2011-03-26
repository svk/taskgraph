(in-package #:cl-gpgi)


#+sbcl
(defun run-program (command-line
		    &key
		    (search t)
		    (input-from-string nil)
		    (input-from-stream nil)
		    (output-to-string nil)
		    (output-to-stream nil))
  (let ((program (car command-line))
	(arguments (cdr command-line)))
    (when (or (and input-from-string input-from-stream)
	      (and output-to-string output-to-stream))
      (error "illegal arguments to run-program: duplicated input or output"))
    (let ((standard-args (list :search search
			       :pty t
			       :wait nil)))
      (flet ((run (input-args)
	       (cond (output-to-string
		      (with-output-to-string (stream)
			(apply #'sb-ext:run-program (append (list program
								  arguments
								  :output stream)
							    standard-args
							    input-args))))
		     (t
		      (progn
			(apply #'sb-ext:run-program (append (list program
								  arguments
								  :output output-to-stream)
							    standard-args
							    input-args))
			nil)))))
	(cond (input-from-string
	       (with-input-from-string (stream input-from-string)
		 (run (list :input stream))))
	      (t
	       (run (list :input input-from-stream))))))))