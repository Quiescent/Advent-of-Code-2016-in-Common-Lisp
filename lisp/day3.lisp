;;; day3 --- My solution to day3 -*-

;;; Commentary:
;; My solution to advent of code: day3

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day3
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day3)

;; # PART 1:

(defun day3-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for (x y z) in input-elements)
    (count (is-valid-triangle x y z))))

(defun is-valid-triangle (x y z)
  (and (> (+ x y) z)
       (> (+ x z) y)
       (> (+ z y) x)))

;; # PART 2:

(defun day3-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((transposed (transpose input-elements))
         (threes     (mapcar #'into-threes transposed)))
    (iter
      (for xss in threes)
      (sum (iter
             (for (x y z) in xss)
             (count (is-valid-triangle x y z)))))))

(defun transpose (xss)
  (apply #'mapcar #'list xss))

(defun into-threes (xss)
  (if (null xss)
      nil
      (cons (list (car xss)
                  (cadr xss)
                  (caddr xss))
            (into-threes (cdddr xss)))))

;; Scratch area:

;; (progn
;;   (print "********** SCRATCH **********
;; ")
;;   (let ((input-1 '())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day3-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day3-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-numbers "day3-part-1"))
        (input-2 (file-lines-numbers "day3-part-1")))
    (format t "
Part 1: ~s
" (day3-part-1 input-1))
    (format t "
Part 2: ~s
" (day3-part-2 input-2))))

