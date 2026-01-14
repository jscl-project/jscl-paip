;;; dev.lisp - Development utility to test loading all PAIP files
;;; Load this file to check which PAIP files load successfully in JSCL

(in-package :cl-user)

;;; PAIP files organized by book chapter
;;; Based on "Paradigms of Artificial Intelligence Programming" by Peter Norvig

(defparameter *paip-chapters*
  '(;; Support files (loaded first)
    (:chapter "Support" :files ("auxfns" "tutor" "examples"))

    ;; Part I: Introduction to Common Lisp
    (:chapter "Ch.1: Introduction to Lisp" :files ("intro"))
    (:chapter "Ch.2: A Simple Lisp Program" :files ("simple"))
    (:chapter "Ch.3: Overview of Lisp" :files ("overview"))

    ;; Part II: Early AI Programs
    (:chapter "Ch.4: GPS: The General Problem Solver" :files ("gps1" "gps"))
    (:chapter "Ch.5: ELIZA: Dialog with a Machine" :files ("eliza1" "eliza" "patmatch" "eliza-pm"))
    (:chapter "Ch.6: Building Software Tools" :files ("search" "gps-srch"))
    (:chapter "Ch.7: STUDENT: Solving Algebra Word Problems" :files ("student"))
    (:chapter "Ch.8: Symbolic Mathematics: A Simplification Program" :files ("macsyma" "macsymar"))

    ;; Part III: Tools and Techniques
    (:chapter "Ch.11: Logic Programming" :files ("unify" "prolog1" "prolog"))
    (:chapter "Ch.12: Compiling Logic Programs" :files ("prologc1" "prologc2" "prologc" "prologcp"))
    (:chapter "Ch.13: Object-Oriented Programming" :files ("clos"))
    (:chapter "Ch.14: Knowledge Representation and Reasoning" :files ("krep1" "krep2" "krep"))

    ;; Part IV: Advanced AI Programs
    (:chapter "Ch.15: Symbolic Mathematics with Canonical Forms" :files ("cmacsyma"))
    (:chapter "Ch.16: Expert Systems" :files ("mycin" "mycin-r"))
    (:chapter "Ch.17: Line-Labeling by Constraint Satisfaction" :files ("waltz"))
    (:chapter "Ch.18: Search and the Game of Othello" :files ("othello" "othello2"))
    (:chapter "Ch.20: Unification Grammars" :files ("syntax1" "syntax2" "syntax3" "unifgram"))
    (:chapter "Ch.21: A Grammar of English" :files ("grammar" "lexicon"))

    ;; Part V: The Rest of Lisp
    (:chapter "Ch.22: Scheme: An Uncommon Lisp" :files ("interp1" "interp2" "interp3"))
    (:chapter "Ch.23: Compiling Lisp" :files ("compile1" "compile2" "compile3" "compopt"))))

(defvar *load-results* nil
  "Association list of (filename . result) where result is :success or an error message")

(defun try-load-file (filename)
  "Attempt to load a PAIP file and return :success or the error message."
  (handler-case
      (progn
        (load filename)
        :success)
    (error (e)
      (format nil "~A" e))))

(defun test-load-all-files ()
  "Try to load all PAIP files and collect results."
  (setq *load-results* nil)
  (setq *paip-modules* nil)  ; Reset loaded modules
  (dolist (chapter-info *paip-chapters*)
    (let ((chapter (getf chapter-info :chapter))
          (files (getf chapter-info :files)))
      (format t "~%~%=== ~A ===~%" chapter)
      (dolist (file files)
        (format t "  Loading ~A... " file)
        (let ((result (try-load-file file)))
          (push (cons file result) *load-results*)
          (if (eq result :success)
              (format t "OK~%")
              (format t "FAILED~%    Error: ~A~%" result))))))
  (print-summary))

(defun print-summary ()
  "Print a summary of load results."
  (let ((successes (remove-if-not (lambda (r) (eq (cdr r) :success)) *load-results*))
        (failures (remove-if (lambda (r) (eq (cdr r) :success)) *load-results*)))
    (format t "~%~%========================================~%")
    (format t "SUMMARY~%")
    (format t "========================================~%")
    (format t "Total files: ~A~%" (length *load-results*))
    (format t "Successful:  ~A~%" (length successes))
    (format t "Failed:      ~A~%" (length failures))
    (when failures
      (format t "~%Failed files:~%")
      (dolist (f (reverse failures))
        (format t "  - ~A~%" (car f))))))

(defun test-single-file (filename)
  "Test loading a single file and report result."
  (format t "Loading ~A... " filename)
  (let ((result (try-load-file filename)))
    (if (eq result :success)
        (format t "OK~%")
        (format t "FAILED~%  Error: ~A~%" result))
    result))

(defun list-files-by-chapter ()
  "Print all PAIP files organized by chapter."
  (dolist (chapter-info *paip-chapters*)
    (let ((chapter (getf chapter-info :chapter))
          (files (getf chapter-info :files)))
      (format t "~%~A~%" chapter)
      (dolist (file files)
        (format t "  - ~A~%" file)))))

;;; Usage instructions
(format t "~%PAIP File Loader Test Utility~%")
(format t "==============================~%")
(format t "Available commands:~%")
(format t "  (test-load-all-files)   - Try loading all PAIP files~%")
(format t "  (test-single-file name) - Try loading a single file~%")
(format t "  (list-files-by-chapter) - List all files by chapter~%")
(format t "  (print-summary)         - Print last test summary~%")
