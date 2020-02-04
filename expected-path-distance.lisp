(in-package :cl)

(defpackage :com.gashu.expected-path-distance
  (:use :cl))

(in-package :com.gashu.expected-path-distance)


(defparameter *n* 0)
(defparameter *graph* 0)
(defparameter *q* 0)
(defparameter *line* nil)
(defparameter *nodes* nil)
(defparameter *num-nodes* 0)

(defclass node ()
  ((dist
    :initarg :dist
    :accessor dist)
   (minimum
    :initarg :minimum
    :accessor minimum)
   (from
    :initarg :from
    :accessor from)
   (to
    :initarg :to
    :accessor to)))

(defun setup ()
  (setf *n* (read))
  (setf *graph* (make-array (list *n*)))
  (loop repeat *n*
	for i = 0 then (1+ i) do
	  (let ((from (read))
		(to (read)))
	    (setf *num-nodes* (*num-nodes* from to))
	    (setf (aref *graph* i) (make-instance 'node :from from :to to :dist (read)))))
  (setf *q* (read))
  (setf *line* (make-array (list *q*)))
  (loop repeat *q*
	for i = 0 then (1+ i) do
	  (setf (aref *line* i) (cons (read) (read))))
  (setf *nodes* (make-array (list bi)))
  (loop repeat biggest-node do
	))


(defun init-graph ()
  (map 'vector (lambda (elt) (setf ))))
