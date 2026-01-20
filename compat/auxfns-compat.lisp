;;; Compatibility shims for functions used by auxfns.lisp that JSCL doesn't provide

(in-package :jscl)

;;; Constants
(defconstant most-positive-fixnum 9007199254740991) ; 2^53 - 1 (JS safe integer)

;;; Pathname support
(defstruct (pathname (:constructor %make-pathname))
  name
  type
  directory
  device
  host
  version)

(defun make-pathname (&key name type directory device host version defaults)
  "Create a pathname."
  (let ((p (%make-pathname)))
    (setf (pathname-name p) (or name (and defaults (pathname-name defaults))))
    (setf (pathname-type p) (or type (and defaults (pathname-type defaults))))
    (setf (pathname-directory p) (or directory (and defaults (pathname-directory defaults))))
    (setf (pathname-device p) (or device (and defaults (pathname-device defaults))))
    (setf (pathname-host p) (or host (and defaults (pathname-host defaults))))
    (setf (pathname-version p) (or version (and defaults (pathname-version defaults))))
    p))

(defun namestring (pathname)
  "Return the full namestring of a pathname."
  (if (stringp pathname)
      pathname
      (let ((dir (pathname-directory pathname))
            (name (pathname-name pathname))
            (type (pathname-type pathname)))
        (concatenate 'string
                     (if dir
                         (format nil "~{~a/~}" (cdr dir))
                         "")
                     (or name "")
                     (if type
                         (concatenate 'string "." type)
                         "")))))

(defun truename (path)
  (if (string= path "")
      (make-pathname)
      path))

;;; File system stubs
(defun probe-file (pathname)
  "Return pathname if file exists (stub - always returns nil)."
  (declare (ignore pathname))
  nil)

(defun file-write-date (pathname)
  "Return the write date of a file (stub - always returns nil)."
  (declare (ignore pathname))
  nil)

(defun ensure-directories-exist (pathname)
  "Ensure directories exist (stub - does nothing in browser)."
  pathname)
