;;; day9 --- My solution to day9 -*-

;;; Commentary:
;; My solution to advent of code: day9

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day9
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day9)

;; # PART 1:

(defvar marker-regex "\\([0-9]+x[0-9]+\\)")

(defun day9-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((str (car input-elements)))
    (iter
      (while (not (equal str "")))
      (for next-match = (car (ppcre:all-matches marker-regex str)))
      (when (null next-match)
        (sum (length str))
        (setf str "")
        (next-iteration))
      (when (not (eq 0 next-match))
        (sum next-match)
        (setf str (subseq str next-match)))
      (for marker-string = (car (ppcre:all-matches-as-strings marker-regex str)))
      (for (len repeats) = (parse-marker marker-string))
      (for extension-length = (* len repeats))
      (setf str (subseq str (+ (length marker-string) len)))
      (sum extension-length))))

(defun parse-marker (marker)
  (mapcar #'read-from-string (remove "" (ppcre:split " " (ppcre:regex-replace-all "[^0-9]" marker " "))
                                     :test #'equal)))

;; # PART 2:

(defun day9-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((str (car input-elements)))
    (iter
      (while (not (equal str "")))
      (for next-match = (car (ppcre:all-matches marker-regex str)))
      (when (null next-match)
        (sum (length str))
        (setf str "")
        (next-iteration))
      (when (not (eq 0 next-match))
        (sum next-match)
        (setf str (subseq str next-match)))
      (for marker-string = (car (ppcre:all-matches-as-strings marker-regex str)))
      (for (len repeats) = (parse-marker marker-string))
      (for sub-segment = (subseq str (length marker-string) (+ (length marker-string) len)))
      (for len-sub-segment = (day9-part-2 (list sub-segment)))
      (for extension-length = (* len-sub-segment repeats))
      (setf str (subseq str (+ (length marker-string) len)))
      (sum extension-length))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("A(1x5)BC"))
        (expected-1 7)
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day9-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day9-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day9-part-1"))
        (input-2 (file-lines "day9-part-1")))
    (format t "
Part 1: ~s
" (day9-part-1 input-1))
    (format t "
Part 2: ~s
" (day9-part-2 input-2))))

