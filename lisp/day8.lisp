;;; day8 --- My solution to day8 -*-

;;; Commentary:
;; My solution to advent of code: day8

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day8
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day8)

;; # PART 1:

(defun day8-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((instructions (mapcar #'parse-line input-elements)))
    (iter
      (with grid = (make-array (list 50 6) :initial-element #\_))
      (for instruction in instructions)
      (case (car instruction)
        (rect   (iter (for x from 0 below (cadr instruction))
                      (iter (for y from 0 below (caddr instruction))
                            (setf (aref grid x y) #\#))))
        (column (iter
                  (with temp-grid = (copy-matrix grid)) ; could be shallow...
                  (with x         = (cadr instruction))
                  (with shift     = (caddr instruction))
                  (for y from 0 below 6)
                  (setf (aref grid x y)
                        (aref temp-grid x (mod (+ y (- 6 shift)) 6)))))
        (row    (iter
                  (with temp-grid = (copy-matrix grid)) ; could be shallow...
                  (with y         = (cadr instruction))
                  (with shift     = (caddr instruction))
                  (for x from 0 below 50)
                  (setf (aref grid x y)
                        (aref temp-grid (mod (+ x (- 50 shift)) 50) y)))))
      (finally (return (iter
                         (for x from 0 below 50)
                         (sum (iter
                                (for y from 0 below 6)
                                (count (eq #\# (aref grid x y)))))))))))

(defun copy-matrix (xs)
  (destructuring-bind (x-dim y-dim) (array-dimensions xs)
    (let ((new-matrix (make-array (list x-dim y-dim))))
      (iter (for x from 0 below x-dim)
            (iter (for y from 0 below y-dim)
                  (setf (aref new-matrix x y)
                        (aref xs x y)))
            (finally (return new-matrix))))))

(defun parse-line (line)
  (let ((numbers (mapcar #'read-from-string (remove "" (ppcre:split "[^0-9]" line) :test #'equal))))
    (cond
      ((search "rotate" line)
       (cons (if (search "column" line) 'column 'row)
             numbers))
      (t (cons 'rect numbers)))))

;; # PART 2:

(defun day8-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((instructions (mapcar #'parse-line input-elements)))
    (iter
      (with grid = (make-array (list 50 6) :initial-element #\_))
      (for instruction in instructions)
      (case (car instruction)
        (rect   (iter (for x from 0 below (cadr instruction))
                      (iter (for y from 0 below (caddr instruction))
                            (setf (aref grid x y) #\#))))
        (column (iter
                  (with temp-grid = (copy-matrix grid)) ; could be shallow...
                  (with x         = (cadr instruction))
                  (with shift     = (caddr instruction))
                  (for y from 0 below 6)
                  (setf (aref grid x y)
                        (aref temp-grid x (mod (+ y (- 6 shift)) 6)))))
        (row    (iter
                  (with temp-grid = (copy-matrix grid)) ; could be shallow...
                  (with y         = (cadr instruction))
                  (with shift     = (caddr instruction))
                  (for x from 0 below 50)
                  (setf (aref grid x y)
                        (aref temp-grid (mod (+ x (- 50 shift)) 50) y)))))
      (finally (print (debug-matrix grid))))))

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
;; " expected-1 (day8-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day8-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day8-part-1"))
        (input-2 (file-lines "day8-part-1")))
    (format t "
Part 1: ~s
" (day8-part-1 input-1))
    (format t "
Part 2: ~s
" (day8-part-2 input-2))))

