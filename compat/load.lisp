(in-package :jscl)

(defun load (filename &key (verbose nil) (print nil))
  "Load a Lisp file from the server using synchronous XHR.
   Files are loaded from the paip/ directory by default.
   The .lisp extension is added automatically if not present."
  (let* ((filename-str (namestring filename))
         (name (if (and (> (length filename-str) 5)
                        (string= (subseq filename-str (- (length filename-str) 5)) ".lisp"))
                   filename-str
                   (concatenate 'string filename-str ".lisp")))
         (xhr (make-new #j:XMLHttpRequest))
         (path (concatenate 'string "paip/" name)))

    ((oget xhr "open") "GET" path nil)
    ((oget xhr "send"))
    (if (= (oget xhr "status") 200)
        (let* ((content (oget xhr "responseText"))
               (stream (make-string-input-stream content))
               (eof (gensym)))
          (when verbose
            (format t "~&; Loading ~a~%" path))
          ;; Read and evaluate forms one at a time
          (loop
            (let ((form (read stream nil eof)))
              (when (eq form eof)
                (return t))
              (let ((result (eval-interactive form)))
                (when print
                  (print result))))))
        (error "Could not load file: ~a (status ~a)" path (oget xhr "status")))))
