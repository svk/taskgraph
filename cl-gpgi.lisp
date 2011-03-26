(in-package #:cl-gpgi)

(defparameter *gpg-binary* #P"/usr/bin/gpg")
(defparameter *use-gpg-agent* t)
(defparameter *use-armored-output* t)

(defconstant +max-passphrase-length+ 4096)

(defun make-passphrase-cache ()
  (let* ((has-passphrase nil)
	 (length nil)
	 (N +max-passphrase-length+)
	 (data (make-array (list N)
			  :element-type 'character)))
    (labels ((clear ()
	       (setf has-passphrase nil
		     length nil)
	       (dotimes (i N)
		 (setf (aref data i) #\@))
	       nil)
	     (begin ()
	       (clear)
	       (setf length 0)
	       nil)
	     (output (stream)
	       (when (not has-passphrase)
		 (error "no cached passphrase"))
	       (dotimes (i length)
		 (princ (aref data i) stream)
		 nil))
	     (has ()
	       has-passphrase)
	     (complete ()
	       (setf has-passphrase t)
	       nil)
	     (appendc (ch)
	       (when (>= length N)
		 (clear)
		 (error "passphrase too long"))
	       (setf (aref data length) ch)
	       (incf length)
	       nil))
      #'(lambda (name &rest rest)
	  (apply (case name
		   (:clear #'clear)
		   (:begin #'begin)
		   (:output #'output)
		   (:complete #'complete)
		   (:has #'has)
		   (:append #'appendc))
		 rest)))))

(defparameter *passphrase-cache* (make-passphrase-cache))

(defun make-base-gpg-cmd ()
  (let ((rv nil))
    (push (if *use-gpg-agent*
	      "--use-agent"
	      "--no-use-agent")
	  rv)
    (when *use-armored-output*
      (push "--armor" rv))
    (push "--with-colons" rv)
    (push "--batch" rv)
    (push *gpg-binary* rv)))

(defun make-gpg-cmd (&rest rest)
  (nconc (make-base-gpg-cmd)
	 rest))

(defun run-gpg (input &rest rest)
  (cond ((streamp input)
	 (run-program (apply #'make-gpg-cmd rest)
		      :search nil
		      :input-from-stream input
		      :output-to-string t))
	((stringp input)
	 (run-program (apply #'make-gpg-cmd rest)
		      :search nil
		      :input-from-string input
		      :output-to-string t))
	(t (error "input was not string or stream"))))

(defun simple-encrypt-to (data recipient)
  (run-gpg data
	   "--encrypt"
	   "--recipient"
	   recipient))

(defun simple-decrypt (data)
  (run-gpg data
	   "--decrypt"))


	    

