(in-package #:cl-gpgi)

#+allegro
(defun run-program (command-line
		    &key
		    (search t)
		    (input-from-string nil)
		    (input-from-stream nil)
		    (output-to-string nil)
		    (output-to-stream nil))
  ;; Mostly untested!
  (when (not search)
    (warn (format nil "cannot avoid searching for ~a in allegro" (car command-line))))
  (let* ((command-line-array (make-array (list (length command-line))
					 :initial-contents command-line))
	 (results (excl.osi:command-output command-line-array
					   :whole t
					   :input (if input-from-stream
						      #'(lambda (ws)
							  (write-line (read-line input-from-stream)
								      ws))
						      input-from-string))))
    (when output-to-string
      (return-from run-program results))
    (when output-to-stream
      (princ results output-to-stream))))

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
			       :error nil
			       :wait t)))
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