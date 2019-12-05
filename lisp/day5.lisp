;;; day5 --- My solution to day5 -*-

;;; Commentary:
;; My solution to advent of code: day5

;;; Code:

(load "~/quicklisp/setup.lisp")

(ql:quickload "iterate")
(ql:quickload "cl-charms")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day5
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day5)

;; # PART 1:

(defun day5-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for i from 0 below 100000000)
    (for hash = (md5 (format nil "~a~a" input-elements i)))
    (when (>= (position-if-not (lambda (c) (eq c #\0)) hash) 5)
      (collect (aref hash 5) into password)
      (when (eq (length password) 8)
        (return (map 'string #'identity password))))))

;; # PART 2:
(defun day5-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((password (make-array '(8) :initial-element #\_))
        (randoms  "0123456789abcdefghijklmnopqrstuvwxyz"))
    (charms:with-curses ()
      (iter
        (sb-ext:seed-random-state)
        (for i from 0 below 100000000)
        (for hash = (md5 (format nil "~a~a" input-elements i)))
        (when (eq 0 (mod i 1000))
          (for printed = (copy-seq password))
          (iter
            (for j from 0 below 8)
            (when (eq #\_ (aref printed j))
              (setf (aref printed j) (aref randoms (random (length randoms))))))
          (with window = (charms:make-window 50 15 10 10))
          (charms:clear-window window)
          (charms:write-string-at-point window (format nil "~a" (map 'string #'identity printed)) 0 0)
          (charms:refresh-window window))
        (when (and (>= (position-if-not (lambda (c) (eq c #\0)) hash) 5)
                   (member (aref hash 5) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))
          (let ((idx (read-from-string (string (aref hash 5)))))
            (when (eq (aref password idx) #\_)
              (setf (aref password idx) (aref hash 6))))
          (when (every (lambda (c) (not (eq c #\_))) password)
            (return (map 'string #'identity password))))))))

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
;; " expected-1 (day5-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day5-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 "cxdnnyjw")
        (input-2 "cxdnnyjw"))
    (format t "
Part 1: ~s
" (day5-part-1 input-1))
    (format t "
Part 2: ~s
" (day5-part-2 input-2))))

