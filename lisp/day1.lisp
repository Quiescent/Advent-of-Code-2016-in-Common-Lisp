;;; day1 --- My solution to day1 -*-

;;; Commentary:
;; My solution to advent of code: day1

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day1
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day1)

;; # PART 1:

(defun day1-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((x 0)
        (y 0)
        (direction 'UP))
    (iter
      (for word in (car input-elements))
      (setf direction (new-direction direction (aref word 0)))
      (for count = (get-number word))
      (cond
        ((eq direction 'UP)    (decf y count))
        ((eq direction 'DOWN)  (incf y count))
        ((eq direction 'RIGHT) (incf x count))
        ((eq direction 'LEFT)  (decf x count)))
      (finally (return (+ (abs x) (abs y)))))))

(defun get-number (str)
  (read-from-string (ppcre:regex-replace-all "[^0-9]" str "")))

(defvar directions
    (progn
      (let ((directions-temp (make-array '(4))))
        (setf (aref directions-temp 0) 'UP)
        (setf (aref directions-temp 1) 'RIGHT)
        (setf (aref directions-temp 2) 'DOWN)
        (setf (aref directions-temp 3) 'LEFT)
        directions-temp)))

(defun new-direction (current-direction turn)
  (cond
    ((eq turn #\L) (aref directions (mod (1- (position current-direction directions)) 4)))
    ((eq turn #\R) (aref directions (mod (1+ (position current-direction directions)) 4)))))

;; # PART 2:

(defun day1-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((x 0)
        (y 0)
        (direction 'UP)
        (visited))
    (iter outer
      (for word in (car input-elements))
      (setf direction (new-direction direction (aref word 0)))
      (for count = (get-number word))
      (for incrementer =
           (cond
             ((eq direction 'UP)    (lambda () (decf y)))
             ((eq direction 'DOWN)  (lambda () (incf y)))
             ((eq direction 'RIGHT) (lambda () (incf x)))
             ((eq direction 'LEFT)  (lambda () (decf x)))))
      (iter
        (for i from 0 below count)
        (funcall incrementer)
        (when (member (cons x y) visited :test #'equal)
          (return-from outer (+ (abs x) (abs y))))
        (push (cons x y) visited)))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '(("R2," "L3")))
        (expected-1 5)
        (input-2 '(("R8," "R4," "R4," "R8")))
        (expected-2 4))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day1-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day1-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-words "day1-part-1"))
        (input-2 (file-lines-words "day1-part-1")))
    (format t "
Part 1: ~s
" (day1-part-1 input-1))
    (format t "
Part 2: ~s
" (day1-part-2 input-2))))

