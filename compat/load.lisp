(in-package :jscl)

(defun load (filename &key (verbose nil) (print nil))
  "Load a Lisp file from the server using synchronous XHR.
   Files are loaded from the paip/ directory by default.
   The .lisp extension is added automatically if not present."
  (declare (ignore print))
  (let* ((name (if (and (> (length filename) 5)
                        (string= (subseq filename (- (length filename) 5)) ".lisp"))
                   filename
                   (concatenate 'string filename ".lisp")))
         (xhr (make-new #j:XMLHttpRequest))
         (path (concatenate 'string "paip/" name)))
    ((oget xhr "open") "GET" path nil)
    ((oget xhr "send"))
    (if (= (oget xhr "status") 200)
        (let ((content (oget xhr "responseText")))
          (when verbose
            (format t "~&; Loading ~a~%" path))
          (jscl::eval-interactive
           (jscl::read-from-string (concatenate 'string "(progn " content ")")))
          t)
        (error "Could not load file: ~a (status ~a)" path (oget xhr "status")))))
