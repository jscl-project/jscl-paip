(in-package :cl)

(defun y-or-n-p (&optional format-string &rest format-arguments)
  "Ask the user a yes-or-no question. Return T for y/Y, NIL for n/N.
Reprompts on invalid input."
  (when format-string
    (fresh-line)
    (apply #'format t format-string format-arguments))
  (format t " (y or n) ")
  (finish-output)
  (loop
    (let ((line (read-line *standard-input* nil)))
      (when line
        (let ((trimmed (string-trim '(#\Space #\Tab) line)))
          (when (= (length trimmed) 1)
            (let ((ch (char-upcase (char trimmed 0))))
              (case ch
                (#\Y (return t))
                (#\N (return nil))))))
        (format t "Please answer y or n: ")
        (finish-output)))))

(defun yes-or-no-p (&optional format-string &rest format-arguments)
  "Ask the user a yes-or-no question. Return T for 'yes', NIL for 'no'.
Reprompts on invalid input."
  (when format-string
    (fresh-line)
    (apply #'format t format-string format-arguments))
  (format t " (yes or no) ")
  (finish-output)
  (loop
    (let ((line (read-line *standard-input* nil)))
      (when line
        (let ((trimmed (string-downcase (string-trim '(#\Space #\Tab) line))))
          (cond
            ((string= trimmed "yes") (return t))
            ((string= trimmed "no") (return nil))
            (t
             (format t "Please answer yes or no: ")
             (finish-output))))))))
