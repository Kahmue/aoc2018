(defpackage #:aoc2018
  (:use :cl :alexandria))

(in-package #:aoc2018)

;;;; Day 1


;;;Input - file #p"~/quicklisp/local-projects/aoc2018/aoc2018_1_input.txt"


(defun read-file (file &optional (fn NIL))
  "Reads a line of input and applys, if given, fn to it. The result is a list of fn'ed input-lines."
  (with-open-file (in (pathname file))
    (loop for line = (read-line in NIL NIL)
       while line
       collect (if fn
		   (funcall fn line)
		   line))))
		   

;;; A

(defun d1a (file)
  (apply #'+ (read-file file)))

;;; B

(defun d1b (input)
  (let ((cache (make-hash-table))
	(sum 0))
    (setf (gethash 0 cache) 't)
    (loop
       do (loop for number in input
	     do
	       (setf sum (+ sum number))
	       (if (gethash sum cache)
		   (return-from d1b sum))
	       (setf (gethash sum cache) t)))))

