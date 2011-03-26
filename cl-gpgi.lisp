(in-package #:cl-gpgi)

(defparameter *gpg-binary* #P"/usr/bin/gpg")
(defparameter *use-gpg-agent* nil)
(defparameter *use-armored-output* t)

(defconstant +max-passphrase-length+ 4096)

(defstruct passphrase-cache
  (has-passphrase? nil)
  (length nil)
  (data (make-array (list +max-passphrase-length+)
		    :element-type 'character)))

(defparameter *passphrase-cache* (make-passphrase-cache))

(defun passphrase-cache-clear (pc)
  (with-slots (length data has-passphrase?)
      pc
    (setf has-passphrase? nil)
    (setf length nil)
    (dotimes (i +max-passphrase-length+)
      (setf (aref data i) #\@))))

(defun passphrase-cache-begin (pc)
  (with-slots (length)
      pc
    (passphrase-cache-clear pc)
    (setf length 0)))

(defun passphrase-cache-write (pc stream)
  (with-slots (has-passphrase? length data)
      pc
    (when (not has-passphrase?)
      (error "no cached passphrase"))
    (dotimes (i (passphrase-cache-length pc))
      (princ (aref data i) stream)
      nil)))
  
(defun passphrase-cache-append (pc ch)
  (with-slots (length data)
      pc
    (when (>= length +max-passphrase-length+)
      (passphrase-cache-clear pc)
      (error "passphrase too long"))
    (setf (aref data length) ch)
    (incf length)
    nil))

(defun passphrase-cache-complete (pc)
  (with-slots (has-passphrase?)
      pc
    (setf has-passphrase? t)))

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
  (run-program (apply #'make-gpg-cmd rest)
	       :search nil
	       :input-from-string input
	       :output-to-string t))

(defun simple-encrypt-to (data recipient)
  (run-gpg data
	   "--encrypt"
	   "--recipient"
	   recipient))

(defun simple-decrypt (data)
  (run-gpg data
	   "--decrypt"))
	   
			     



	    

