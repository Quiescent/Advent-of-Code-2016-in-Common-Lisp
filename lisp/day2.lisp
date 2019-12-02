;;; day2 --- My solution to day2 -*-

;;; Commentary:
;; My solution to advent of code: day2

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day2
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day2)

;; # PART 1:

(defun code-at-grid (x y)
  (cond
    ((equal (cons x y) '(0 . 0)) #\1)
    ((equal (cons x y) '(1 . 0)) #\2)
    ((equal (cons x y) '(2 . 0)) #\3)
    ((equal (cons x y) '(0 . 1)) #\4)
    ((equal (cons x y) '(1 . 1)) #\5)
    ((equal (cons x y) '(2 . 1)) #\6)
    ((equal (cons x y) '(0 . 2)) #\7)
    ((equal (cons x y) '(1 . 2)) #\8)
    ((equal (cons x y) '(2 . 2)) #\9)))

(defun zero-if-negative (x)
  (if (< x 0) 0 x))

(defun saturate-at-two (x)
  (if (> x 2) 2 x))

(defun day2-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (map 'string #'identity
       (iter
         (with x = 1)
         (with y = 1)
         (for code-line in input-elements)
         (iter
           (for instruction in-string code-line)
           (case instruction
             (#\L (setf x (zero-if-negative (1- x))))
             (#\U (setf y (zero-if-negative (1- y))))
             (#\D (setf y (saturate-at-two  (1+ y))))
             (#\R (setf x (saturate-at-two  (1+ x))))))
         (collect (code-at-grid x y)))))

;; # PART 2:

;;     1
;;   2 3 4
;; 5 6 7 8 9
;;   A B C
;;     D

(defvar big-grid
    '((nil nil #\1 nil nil)
      (nil #\2 #\3 #\4 nil)
      (#\5 #\6 #\7 #\8 #\9)
      (nil #\A #\B #\C nil)
      (nil nil #\D nil nil)))

(defun big-grid-at (x y)
  (nth x (nth y big-grid)))

(defun day2-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (map 'string #'identity
       (iter
         (with x = 0)
         (with y = 2)
         (for code-line in input-elements)
         (iter
           (for instruction in-string code-line)
           (with new-x = x)
           (with new-y = y)
           (with next-x = new-x)
           (with next-y = new-y)
           (case instruction
             (#\L (decf next-x))
             (#\U (decf next-y))
             (#\D (incf next-y))
             (#\R (incf next-x)))
           (when (or (< next-x 0)
                     (< next-y 0)
                     (> next-x 4)
                     (> next-y 4)
                     (null (big-grid-at next-x next-y)))
             (setf next-x new-x
                   next-y new-y))
           (setf new-x next-x
                 new-y next-y)
           (finally (setf x new-x
                          y new-y)))
         (collect (big-grid-at x y)))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("ULL"
                   "RRDDD"
                   "LURDL"
                   "UUUUD"))
        (expected-1 '())
        (input-2 '("ULL"
                   "RRDDD"
                   "LURDL"
                   "UUUUD"))
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day2-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day2-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day2-part-1"))
        (input-2 (file-lines "day2-part-1")))
    (format t "
Part 1: ~s
" (day2-part-1 input-1))
    (format t "
Part 2: ~s
" (day2-part-2 input-2))))

