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

;;;; Day 2

;;; A
(defun find-multiples (input)
  (let ((counter (make-hash-table))
	(result NIL))
    (loop for c across input do
	 (incf (gethash c counter 0)))
    (setf result (loop for v being the hash-values in counter collect v))
    (values (> (count-if #'(lambda (x) (= x 2)) result) 0)
	    (> (count-if #'(lambda (x) (= x 3)) result) 0))))

(defun d2a (input)
  (let ((doubles 0)
	(triples 0))
    (loop :for id :in input
       :do
       (multiple-value-bind (double triple) (find-multiples id)
	 (if double (incf doubles))
	 (if triple (incf triples))))
    (* doubles triples)))

;;; B

(defun diff-id (id1 id2)
  (let ((result (make-array 1 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop :for index :from 0 :below (length id1)
       :do
	 (when (char= (aref id1 index) (aref id2 index))
	    (vector-push-extend (aref id1 index) result)))
    result))

(defun d2b (input)
  (if (null input) NIL)
  (let* ((first-id (car input))
	 (len (length first-id)))
    (loop :for id :in (cdr input)
       :do
	 (let ((diff (diff-id first-id id)))
	   (if (= (- len (length diff)) 1)
	       (return-from d2b diff))))
    (d2b (cdr input))))
