(in-package :cl)

(defpackage :com.gashu.expected-path-distance
  (:use :cl))

(in-package :com.gashu.expected-path-distance)

(declaim (optimize (debug 3)))

(defparameter *n* 0)
(defparameter *graph* (make-hash-table))
(defparameter *q* 0)
(defparameter *line* nil)
(defparameter *nodes* nil)
(defparameter *nodes-set* nil)
(defparameter *distances* nil)
(defparameter *min-path* nil)
(defparameter *a* nil)
(defparameter *b* nil)
(defparameter *cur-node-set* nil)
(defparameter *queue* nil)

(defclass node ()
  ((dist
    :initarg :dist
    :accessor dist
    :initform most-positive-fixnum)
   (minimum
    :initarg :minimum
    :accessor minimum
    :initform (- 1))
   (neighbors
    :initarg :neighbors
    :accessor neighbors
    :initform nil)
   (num
    :initarg :num
    :accessor num)))


(defun insert-ordered (ordered-list elt)
  (if (null ordered-list)
      (cons elt nil)
      (if (> elt (car ordered-list))
	  (cons (car ordered-list) (insert-ordered (cdr ordered-list) elt))
	  (cons elt ordered-list))))


(defun pop-list (ordered-list)
  (values (cdr ordered-list) (car ordered-list)))

(defun get-neighbor-dest (neighbor)
  (car (car neighbor)))

(defun get-neighbor-weight (neighbor)
  (cdr (car neighbor)))

(defparameter *test-neighbor* (list (cons 0 2) (cons 4 8) (cons 3 33)))

(search-node *test-neighbor* 4)

(defun search-node (node-list node)
  (if (null node-list)
      nil
      (if (= (get-neighbor-dest node-list) node)
	  node
	  (search-node (cdr node-list) node))))

(defun add-node (graph from to weight)
  (let ((node (gethash from graph)))
    (if (null node)
	(setf (gethash from graph) (make-instance 'node :num from :neighbors (list (cons to weight))))
	(with-accessors ((neighbors neighbors)) node
	  (if (nul (search-node neighbors to))
	      (setf neighbors (cons (cons to weight) neighbors))
	      node)))))

(defparameter *teste* (make-hash-table))
(setf (gethash 0 *teste*) (make-instance 'node))
(let ((graph *teste*))
  (with-accessors ((neighbors neighbors)) (gethash 0 *teste*)
  (setf neighbors (cons (cons 0 1) neighbors))))


(defun setup ()
  (setf *n* (read))
  (setf *graph* (make-array (list *n*)))
  (loop repeat *n*
	for i = 0 then (1+ i) do
	  (let ((from (read))
		(to (read))
		(weight (read)))
	    (setf (aref *graph* from) (add-node *graph* from to weight))))
  (setf *q* (read))
  (setf *line* (make-array (list *q*)))
  (loop repeat *q*
	for i = 0 then (1+ i) do
	  (setf (aref *line* i) (cons (read) (read)))))

(defun setup-from-file ()
  (with-open-file (in "expected-path-distance.data" :direction :input)
    (setf *n* (1- (read in)))
    (setf *graph* (make-hash-table))
    (loop repeat *n*
	  for i = 0 then (1+ i) do
	    (let ((from (read in))
		  (to (read in))
		  (weight (read in)))
	      (setf (gethash from *graph*) (add-node *graph* from to weight))
	      (setf (gethash from *graph*) (add-node *graph* to from weight))))
    (setf *q* (read in))
    (setf *line* (make-array (list *q*)))
    (loop repeat *q*
	  for i = 0 then (1+ i) do
	    (progn (setf (aref *line* i) (cons (read in) (read in)))))))

(aref *line* 0)

(setup-from-file)
(init-graph 0)
(loop repeat 3 do
      (format t "iu"))

(defun init-graph (problem-num)
  (setf *a* (car (aref *line* problem-num)))
  (setf *b* (cdr (aref *line* problem-num)))
  (loop for k being the hash-key of *graph* using (hash-value v) do
    (progn
      (unless (null v)
	(format t "k: ~a v: ~a ~%" k v)
	(setf (dist v) most-positive-fixnum)
	(setf (minimum  v) (- 1)))))
  (format t "a: ~a b: ~a ~%" *a* *b*)
  (setf (dist (gethash *a* *graph*)) 0)
  (setf *queue* (insert-ordered *queue* *a*)))

(defparameter *teste* (make-hash-table))
(setf (gethash 0 *teste*) (make-instance 'node))
(setf (dist (gethash 0 *teste*)) 0)

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
