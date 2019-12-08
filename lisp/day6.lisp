;;; day6 --- My solution to day6 -*-

;;; Commentary:
;; My solution to advent of code: day6

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day6
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day6)

;; # PART 1:

(defun day6-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (with counts = (map 'vector #'identity
                        (make-array (list (length (car input-elements))))))
    (initially (iter (for i from 0 below (length (car input-elements)))
                     (setf (aref counts i) (make-hash-table :test #'equal))))
    (for line in input-elements)
    (iter
      (for char in-string line)
      (for i from 0)
      (incf (gethash char (aref counts i) 0)))
    (finally (return (map 'string #'identity
                          (iter (for count-table in-vector counts)
                                (collect (iter (for (key value) in-hashtable count-table)
                                               (finding key maximizing value)))))))))

;; # PART 2:

(defun day6-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (with counts = (map 'vector #'identity
                        (make-array (list (length (car input-elements))))))
    (initially (iter (for i from 0 below (length (car input-elements)))
                     (setf (aref counts i) (make-hash-table :test #'equal))))
    (for line in input-elements)
    (iter
      (for char in-string line)
      (for i from 0)
      (incf (gethash char (aref counts i) 0)))
    (finally (return (map 'string #'identity
                          (iter (for count-table in-vector counts)
                                (collect (iter (for (key value) in-hashtable count-table)
                                               (finding key minimizing value)))))))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '())
        (expected-1 '())
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day6-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day6-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day6-part-1"))
        (input-2 (file-lines "day6-part-1")))
    (format t "
Part 1: ~s
" (day6-part-1 input-1))
    (format t "
Part 2: ~s
" (day6-part-2 input-2))))

