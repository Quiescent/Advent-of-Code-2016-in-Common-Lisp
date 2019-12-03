;;; day4 --- My solution to day4 -*-

;;; Commentary:
;; My solution to advent of code: day4

;;; Code:

(ql:quickload "iterate")
(ql:quickload "str")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day4
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day4)

;; # PART 1:

(defun day4-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for room-id in (mapcar #'parse-room-id input-elements))
    (for letters = (apply #'concatenate 'list (subseq room-id 0 (- (length room-id) 2))))
    (for counts = (make-hash-table))
    (sum (iter
             (for letter in letters)
             (incf (gethash letter counts 0))
             (finally
              (let ((most-common (concatenate 'string
                                              (mapcar #'cdr
                                                      (stable-sort (sort (iter (for (key value) in-hashtable counts)
                                                                               (collect (cons value key)))
                                                                         #'char-lessp :key #'cdr)
                                                                   #'> :key #'car)))))
                (return (if (equal (subseq most-common 0 5) (car (last room-id)))
                            (read-from-string (nth (- (length room-id) 2) room-id))
                            0))))))))

(defun parse-room-id (str)
  (str:words (ppcre:regex-replace-all "[-\\[\\]]" str " ")))

;; # PART 2:

(defun day4-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for room-id in input-elements)
    (for cipher-key = (parse-cipher-key room-id))
    (print
     (iter
       (for char in-string room-id)
       (collect (if (eq char #\-)
                    #\ 
                    (decrypt char cipher-key))
        result-type string)))))

(defun decrypt (char key)
  (if (not (member char (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
      char
      (let* ((int-char (case char
                         (#\a 0)
                         (#\b 1)
                         (#\c 2)
                         (#\d 3)
                         (#\e 4)
                         (#\f 5)
                         (#\g 6)
                         (#\h 7)
                         (#\i 8)
                         (#\j 9)
                         (#\k 10)
                         (#\l 11)
                         (#\m 12)
                         (#\n 13)
                         (#\o 14)
                         (#\p 15)
                         (#\q 16)
                         (#\r 17)
                         (#\s 18)
                         (#\t 19)
                         (#\u 20)
                         (#\v 21)
                         (#\w 22)
                         (#\x 23)
                         (#\y 24)
                         (#\z 25)))
             (shifted (mod (+ int-char key) 26)))
        (case shifted
          (0 #\a)
          (1 #\b)
          (2 #\c)
          (3 #\d)
          (4 #\e)
          (5 #\f)
          (6 #\g)
          (7 #\h)
          (8 #\i)
          (9 #\j)
          (10 #\k)
          (11 #\l)
          (12 #\m)
          (13 #\n)
          (14 #\o)
          (15 #\p)
          (16 #\q)
          (17 #\r)
          (18 #\s)
          (19 #\t)
          (20 #\u)
          (21 #\v)
          (22 #\w)
          (23 #\x)
          (24 #\y)
          (25 #\z)))))

(defun parse-cipher-key (str)
  (read-from-string (car (str:words (ppcre:regex-replace-all "[^0-9]" str " ")))))

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
;; " expected-1 (day4-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day4-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day4-part-1"))
        (input-2 (file-lines "day4-part-1")))
    (format t "
Part 1: ~s
" (day4-part-1 input-1))
    (format t "
Part 2: ~s
" (day4-part-2 input-2))))

