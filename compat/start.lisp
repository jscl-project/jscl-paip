(setq *package* (find-package :cl-user))

(format t "~%Welcome to JSCL-PAIP!~%")
(format t "Paradigms of Artificial Intelligence Programming in the browser.~%")
(format t "~%")

(format t "Loading auxfns.lisp...~%")
(load "auxfns")
(format t "Done. Use (requires 'filename) to load other PAIP files.~%")
(format t "~%")
