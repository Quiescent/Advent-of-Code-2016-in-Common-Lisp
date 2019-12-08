;;; day7 --- My solution to day7 -*-

;;; Commentary:
;; My solution to advent of code: day7

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day7
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day7)

;; # PART 1:

(defun day7-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for ip in input-elements)
    (count (supports-tls ip))))

(defun supports-tls (ip)
  (and
   (not (iter
          (for char in-string (apply #'concatenate 'string (subgroups-only ip)))
          (for p-char previous char)
          (for pp-char previous p-char)
          (for ppp-char previous pp-char)
          (when (and (eq char ppp-char)
                     (eq p-char pp-char)
                     (not (eq char p-char)))
            (return t))
          (finally (return nil))))
   (iter
     (for char in-string (ppcre:regex-replace-all "\\[[a-z]*\\]" ip "-"))
     (for p-char previous char)
     (for pp-char previous p-char)
     (for ppp-char previous pp-char)
     (when (and (eq char ppp-char)
                (eq p-char pp-char)
                (not (eq char p-char)))
       (return t))
     (finally (return nil)))))

(defun subgroups-only (ip)
  (ppcre:all-matches-as-strings "\\[[a-z]*\\]" ip))

;; # PART 2:

(defun day7-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for ip in input-elements)
    (count (supports-ssl ip))))

(defun supports-ssl (ip)
  (let ((subgroups (apply #'concatenate 'string (subgroups-only ip))))
    (iter
      (for char in-string (ppcre:regex-replace-all "\\[[a-z]*\\]" ip "-"))
      (for p-char previous char)
      (for pp-char previous p-char)
      (when (and (eq char pp-char)
                 (not (eq char p-char))
                 (search (map 'string #'identity (list p-char char p-char))
                         subgroups))
        (return t))
      (finally (return nil)))))

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
" expected-1 (day7-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day7-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day7-part-1"))
        (input-2 (file-lines "day7-part-1")))
    (format t "
Part 1: ~s
" (day7-part-1 input-1))
    (format t "
Part 2: ~s
" (day7-part-2 input-2))))

