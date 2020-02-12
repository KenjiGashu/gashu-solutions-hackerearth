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
    :accessor num)
   (visited
    :initarg :visited
    :accessor visited
    :initform nil)
   (path
    :initarg :path
    :accessor path
    :initform nil)))

 (defmethod print-object ((obj node) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (format stream "NODE: ~%  dist: ~a~%  minimum: ~a~%  neighbors: ~a~%  node-num: ~a~%  visited: ~a~%"
	     (dist obj) (minimum obj) (neighbors obj) (num obj) (visited obj))))

(defun insert-ordered (ordered-list elt)
  (if (null ordered-list)
      (cons elt nil)
      (if (> elt (car ordered-list))
	  (cons (car ordered-list) (insert-ordered (cdr ordered-list) elt))
	  (cons elt ordered-list))))

(defparameter *test-insert* nil)
(setf *test-insert* (insert-ordered *test-insert* 4))
(setf *test-insert* (insert-ordered *test-insert* 8))
(setf *test-insert* (insert-ordered *test-insert* 7))
(setf *test-insert* (insert-ordered *test-insert* 9))
(setf *test-insert* (insert-ordered *test-insert* 9))


(defun pop-list (ordered-list)
  (values (cdr ordered-list) (car ordered-list)))

(defun get-neighbor-dest (neighbor)
  (car (car neighbor)))

(defun get-neighbor-weight (neighbor)
  (cdr (car neighbor)))

(defun search-node (node-list node)
  (format t "search node ~a on nodelist: ~a~%" node node-list)
  (if (null node-list)
      nil
      (if (= (get-neighbor-dest node-list) node)
	  node
	  (search-node (cdr node-list) node))))

(defun add-node (graph from to weight)
  (declare (optimize (debug 3)))
  (format t "add node... from: ~a to: ~a weight: ~a~%" from to weight)
  (let ((node (gethash from graph)))
    (if (null node)
	(progn
	  (format t "new node! ~a~%" from)
	  (make-instance 'node :num from :neighbors (list (cons to weight))))
	(with-accessors ((neighbors neighbors) (num num)) node
	  (format t "number: ~a from: ~a to: ~a~%" num from to)
	  (if (null (search-node neighbors to))
	      (progn
		(setf neighbors (cons (cons to weight) neighbors))
		(format t "adicionado vizinho! ~a ~%" neighbors)
		node) ;;neighbor already exists
	      node)))))

(defun setup ()
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
	    (progn (setf (aref *line* i) (cons (read in) (read in))))))

(defun setup-from-file ()
  (declare (optimize (debug 3)))
  (with-open-file (in "expected-path-distance.data" :direction :input)
    (setf *n* (1- (read in)))
    (setf *graph* (make-hash-table))
    (loop repeat *n*
	  for i = 0 then (1+ i) do
	    (let ((from (read in))
		  (to (read in))
		  (weight (read in)))
	      (format t "adding node... from: ~a to: ~a weight: ~a~%" from to weight)
	      (setf (gethash from *graph*) (add-node *graph* from to weight))
	      (setf (gethash to *graph*) (add-node *graph* to from weight))))
    (setf *q* (read in))
    (setf *line* (make-array (list *q*)))
    (loop repeat *q*
	  for i = 0 then (1+ i) do
	    (progn (setf (aref *line* i) (cons (read in) (read in)))))))

(defun init-graph (problem-num)
  (format t "init graph!~%")
  (setf *a* (car (aref *line* problem-num)))
  (setf *b* (cdr (aref *line* problem-num)))
  (loop for k being the hash-key of *graph* using (hash-value v) do
    (progn
      (unless (null v)
	;;(format t "k: ~a v: ~a ~%" k v)
	(setf (dist v) most-positive-fixnum)
	(setf (minimum  v) (- 1))
	(setf (visited v) nil))))
  ;;(format t "a: ~a b: ~a ~%" *a* *b*)
  (setf (dist (gethash *a* *graph*)) 0)
  (setf *queue* (insert-ordered *queue* *a*))
  (format t "queue: ~a~%" *queue*)
  (setf (path (gethash *a* *graph*)) (list *a*)))

(defgeneric calc-expected-value (grap node))
(defmethod calc-expected-value (graph (node node))
  (with-accessors ((path path)) node
    (let ((len (length path))
	  (resp 0))
      (loop for n in path do
	(let ((current (gethash n graph)))
	  (with-accessors ((dist dist)) current
	    (+ resp dist) )))
      (mod (/ resp len) (+ (expt 10 9) 7)))))




(defun solve ()
  (loop repeat *q*
	for i = 0 then (1+ i) do
	  (progn (init-graph i)
		 (loop do
		   (progn
		     (multiple-value-bind (queue node)
			 (pop-list *queue*)
		       (setf (visited (gethash node *graph*)) t)
		       (setf *queue* queue)
		       (format t "current: ~a neihgborhs: ~a~%" node (neighbors (gethash node *graph*)))
		       (loop for neighbor in (neighbors (gethash node *graph*))
			     do
				(format t "visiting node: ~a~%" neighbor)
				(let ((adjacent (gethash (car neighbor) *graph* ))
				      (current (gethash node *graph* )))
				  (format t "node: ~a saved-best: ~a current: ~a~%" (car neighbor) (dist adjacent) (+ (dist current) (cdr neighbor)))
				  (cond ((> (dist adjacent) (+ (dist current) (cdr neighbor)))
					 (setf (dist adjacent) (+ (dist current) (cdr neighbor)))
					 (setf (minimum adjacent) (num current))
					 (setf (path adjacent) (cons (num adjacent) (path current)))
					 (format t "UPDATE! node: ~a dist: ~a minimum: ~a~%" neighbor (dist adjacent) (minimum adjacent))
					 (format t "did it update? ADJACENT ~a~%" (dist adjacent))
					 (format t "did it update? GETHASH ~a~%" (dist (gethash (car neighbor) *graph*)))
					 ))
				  (unless (visited adjacent)
				    (setf *queue* (insert-ordered *queue* (car neighbor)))
				    (format t "queue: ~a~%" *queue*))))))
		       while (not (null *queue*)))
		 (format t "a: ~a b: ~a~%" (gethash *a* *graph*) (gethash *b* *graph*)))))

;; testes

(defun show-all-nodes (graph)
  (loop for key being the hash-keys of graph
	  using (hash-value value) do
	    (format t "key: ~a no: ~a~%" key value)))

(setup-from-file)
(init-graph 0)
(loop repeat 3 do
      (format t "iu"))

(defparameter *teste* (make-hash-table))
(setf (gethash 0 *teste*) (make-instance 'node))
(let ((graph *teste*))
  (with-accessors ((neighbors neighbors)) (gethash 0 *teste*)
  (setf neighbors (cons (cons 0 1) neighbors))))

(defparameter *teste* (make-hash-table))
(setf (gethash 0 *teste*) (make-instance 'node))
(setf (dist (gethash 0 *teste*)) 0)

(defparameter *test-neighbor* (list (cons 0 2) (cons 4 8) (cons 3 33)))

(search-node *test-neighbor* 4)

(show-all-nodes *graph*)

(solve)



;; response tests

(defparameter *mock-response-node* (make-instance 'node :path (6 4 3 5 9 10) ))
