(defpackage #:aoc2018
  (:use :cl :alexandria))

(in-package #:aoc2018)

;;;; Day 1

;;; A

(defun d1a (file)
  ;; #p"~/quicklisp/local-projects/aoc2018/aoc2018_1_a_input.txt"
  (apply #'+ (with-open-file (in file)
	       (loop for line = (read-line in NIL NIL)
		  while line
		  collect (parse-integer line)))))
