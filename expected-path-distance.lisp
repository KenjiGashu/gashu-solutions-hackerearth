(in-package :cl)

(defpackage :com.gashu.expected-path-distance
  (:use :cl))

(in-package :com.gashu.expected-path-distance)


(defparameter *n* 0)
(defparameter *graph* 0)
(defparameter *q* 0)
(defparameter *line* nil)
(defparameter *nodes* nil)
(defparameter *nodes-set* 0)
(defparameter *distances* nil)
(defparameter *min-path* nil)
(defparameter *a* nil)
(defparameter *b* nil)
(defparameter *cur-node-set* nil)
(defparameter *queue* nil)

(defclass node ()
  ((dist
    :initarg :dist
    :accessor dist)
   (minimum
    :initarg :minimum
    :accessor minimum)
   (neighbors
    :initarg :neighbors
    :accessor neighbors)
   (number
    :initarg :number
    :accessor number)))


(defun insert-ordered (ordered-list elt)
  (if (null ordered-list)
      (cons elt nil)
      (if (> elt (car ordered-list))
	  (cons (car ordered-list) (insert-ordered (cdr ordered-list) elt))
	  (cons elt ordered-list))))


(defun pop-list (ordered-list)
  (values (cdr ordered-list) (car ordered-list)))

(defun add-node (graph from to weight)
  (let ((node (aref graph from)))
    (if (null node)
	(make-instance 'node :from from :to to :neighbors (list (cons to weight)))
	(with-accessors ((neighbors neighbors)) node
	  (setf neighbors (cons (cons to weight) neighbors))))))


(defun setup ()
  (setf *n* (read))
  (setf *graph* (make-array (list *n*)))
  (loop repeat *n*
	for i = 0 then (1+ i) do
	  (let ((from (read))
		(to (read))
		(weight (read)))
	    (setf *num-nodes* (adjoin from *nodes-set*))
	    (setf *num-nodes* (adjoin to *nodes-set*))
	    (setf (aref *graph* from) (add-node *graph* from to weight))))
  (setf *q* (read))
  (setf *line* (make-array (list *q*)))
  (loop repeat *q*
	for i = 0 then (1+ i) do
	  (setf (aref *line* i) (cons (read) (read)))))

(defun setup-from-file ()
  (with-open-file (in "expected-path-distance.data" :direction :input)
    (setf *n* (read in))
    (setf *graph* (make-array (list *n*)))
    (loop repeat *n*
	  for i = 0 then (1+ i) do
	    (let ((from (read in))
		  (to (read in))
		  (weight (read in)))
	      (setf *num-nodes* (adjoin from *nodes-set*))
	      (setf *num-nodes* (adjoin to *nodes-set*))
	      (setf (aref *graph* from) (add-node *graph* from to weight))))
    (setf *q* (read))
    (setf *line* (make-array (list *q*)))
    (loop repeat *q*
	  for i = 0 then (1+ i) do
	    (setf (aref *line* i) (cons (read in) (read in))))))


(defun init-graph (problem-num)
  (setf *a* (car (nth problem-num *line*)))
  (setf *b* (cdr (nth problem-num *line*)))
  (setf *distances* (make-array (list (length *nodes-set*))))
  (setf *min-path* (make-array (list (length *nodes-set*))))
  (loop for node in *nodes-set* do
    (progn (setf (aref *distances* node) (most-positive-fixnum))
	   (setf (aref *min-path* node) -1)))
  (setf (aref *distances* *a*) 0)
  (setf *cur-node-set* (remove *a* *nodes-set*))
  (setf *queue* (insert-ordered *queue* *a*)))

(defun solve ()
  (loop repeat *q*
	for i = 0 then (1+ i) do
	  (progn (init-graph i)
		 (loop do
		   (progn
		     (multiple-value-bind (queue node)
			 (pop-list *queue*)
		       (setf *queue* queue)
		       (loop for neighbor in (neighbors node)
			     do
				(let ((adjacent (aref *nodes-set* (car neighbor))))
				  (cond ((> (dist adjacent) (+ (dist node) (cdr neighbor)))
					 (setf (dist adjacent) (+ (dist node) (cdr neighbor)))
					 (setf (minimum adjacent) (num node)))))))
		     
			  )
		   (while (not (null *queue*)))))))



(setup-from-file )
