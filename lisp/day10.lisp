;;; day10 --- My solution to day10 -*-

;;; Commentary:
;; My solution to advent of code: day10

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day10
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day10)

;; # PART 1:

(defun day10-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((seeds        (parse-seeds        input-elements))
         (instructions (parse-instructions input-elements))
         (bots         (make-hash-table))
         (bot-nums     (bot-nums instructions)))
    (iter
      (for (value . bot-num) in seeds)
      (push value (gethash bot-num bots)))
    (iter outer
      (for i from 0 below 1000)
      (with changed = nil)
      (iter
        (for bot in bot-nums)
        (for values = (gethash bot bots))
        (when (= (length values) 2)
          (for minimum = (reduce #'min values))
          (for maximum = (reduce #'max values))
          (when (and (= maximum 61)
                     (= minimum 17))
            (return-from outer bot))
          (setf (gethash bot bots) nil)
          (for ((type-low . dest-low) type-high . dest-high) = (gethash bot instructions))
          (case type-low
            (bot (push minimum (gethash dest-low bots))))
          (case type-high
            (bot (push maximum (gethash dest-high bots))))
          (setf changed t)))
      (while changed)
      (setf changed nil))))

(defun bot-nums (instructions)
  (iter
    (for (bot-num ((low-type . low-num) high-type . high-num)) in-hashtable instructions)
    (adjoining bot-num)
    (adjoining low-num)
    (adjoining high-num)))

(defun parse-instructions (lines)
  (iter
    (with instructions = (make-hash-table))
    (for line in lines)
    (when (not (search "value" line :start2 0 :end2 (length "value")))
      (for matches = (ppcre:all-matches-as-strings "[a-z]+ [0-9]+" line))
      (for (bot num type-low num-low type-high num-high) =
           (mapcar #'read-from-string
                   (iter (for match in matches)
                         (appending (ppcre:split " " match)))))
      (setf (gethash num instructions) (cons (cons type-low num-low)
                                             (cons type-high num-high))))
    (finally (return instructions))))

(defun parse-seeds (lines)
  (iter
    (for line in lines)
    (when (search "value" line :start2 0 :end2 (length "value"))
      (for (val bot) = (mapcar #'read-from-string
                               (ppcre:all-matches-as-strings "[0-9]+" line)))
      (collect (cons val bot)))))

;; # PART 2:

(defun day10-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((seeds        (parse-seeds        input-elements))
         (instructions (parse-instructions input-elements))
         (bots         (make-hash-table))
         (bot-nums     (bot-nums instructions))
         (outputs      (make-hash-table)))
    (iter
      (for (value . bot-num) in seeds)
      (push value (gethash bot-num bots)))
    (iter
      (for i from 0 below 1000)
      (with changed = nil)
      (iter
        (for bot in bot-nums)
        (for values = (gethash bot bots))
        (when (= (length values) 2)
          (for minimum = (reduce #'min values))
          (for maximum = (reduce #'max values))
          (setf (gethash bot bots) nil)
          (for ((type-low . dest-low) type-high . dest-high) = (gethash bot instructions))
          (case type-low
            (bot    (push minimum (gethash dest-low bots)))
            (output (setf (gethash dest-low outputs) minimum)))
          (case type-high
            (bot    (push maximum (gethash dest-high bots)))
            (output (setf (gethash dest-high outputs) maximum)))
          (setf changed t)))
      (while changed)
      (setf changed nil)
      (finally (progn
                 (return (* (gethash 0 outputs)
                           (gethash 1 outputs)
                           (gethash 2 outputs))))))))

;; Scratch area:

;; (progn
;;   (print "********** SCRATCH **********
;; ")
;;   (let ((input-1 '("value 5 goes to bot 2"
;;                    "bot 2 gives low to bot 1 and high to bot 0"
;;                    "value 3 goes to bot 1"
;;                    "bot 1 gives low to output 1 and high to bot 0"
;;                    "bot 0 gives low to output 2 and high to output 0"
;;                    "value 2 goes to bot 2"))
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day10-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day10-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day10-part-1"))
        (input-2 (file-lines "day10-part-1")))
    (format t "
Part 1: ~s
" (day10-part-1 input-1))
    (format t "
Part 2: ~s
" (day10-part-2 input-2))))

