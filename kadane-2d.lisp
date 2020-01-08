(in-package :cl)

(defpackage :kadane-2d
  (:use :cl))

(in-package :kadane-2d)

(defun prefix-array (array)
  (declare (optimize (speed 3) (safety 0))
	   (type (array fixnum 2) array))
  (let* ((row-len (array-dimension array 0))
	 (col-len (array-dimension array 1))
	 (prefix (make-array (list row-len col-len) :element-type 'fixnum)))
    (declare (type (array fixnum 2) prefix)
	     (type fixnum row-len col-len))
    (dotimes (row row-len)
      (setf (aref prefix row 0) (aref array row 0))
      (loop for col from 1 upTo (1- col-len) do
	(setf (aref prefix row col) (+ (aref prefix row (1- col))
				       (aref array row col)))))
    prefix))

(defun calc-area (prefix k i j)
  (declare (type (array fixnum 2) prefix)
	   (type fixnum i j k))
  (the fixnum (- (aref prefix k j)
		 (aref prefix k i))))

(defun kadane-2d (array)
  (declare (type (array fixnum 2) array))
  (let* ((prefix (prefix-array array))
	 (max-ending-here most-negative-fixnum)
	 (max-so-far most-negative-fixnum)
	 (row-len (array-dimension array 0))
	 (col-len (array-dimension array 1)))
    (declare (type (array fixnum 2) prefix)
	     (type fixnum current-sum max-sum))
    (loop for i from 0 upto (1- col-len) do
      (loop for j from i upto (1- col-len) do
	(when (> (abs (- i j)) 0)
	  (setf max-ending-here (calc-area prefix 0 i j))
	  (loop for k from 1 upto (1- row-len) do
	    (progn (setf max-ending-here (max (calc-area prefix k i j) (+ max-ending-here (calc-area prefix k i j))))
		   (setf max-so-far (max max-so-far max-ending-here)))))))
    max-so-far))

(defun initialize (comparator)
  (if (= (funcall comparator most-positive-fixnum most-negative-fixnum) most-positive-fixnum)
      most-negative-fixnum
      most-positive-fixnum))

(defun generic-kadane-2d (array comparator)
  (declare (type (array fixnum 2) array))
  (let* ((prefix (prefix-array array))
	 (max-ending-here (initialize comparator))
	 (max-so-far (initialize comparator))
	 (row-len (array-dimension array 0))
	 (col-len (array-dimension array 1)))
    (declare (type (array fixnum 2) prefix)
	     (type fixnum current-sum max-sum))
    (loop for i from 0 upto (1- col-len) do
      (loop for j from i upto (1- col-len) do
	(when (> (abs (- i j)) 0)
	  (setf max-ending-here (calc-area prefix 0 i j))
	  (loop for k from 1 upto (1- row-len) do
	    (progn (setf max-ending-here (funcall comparator (calc-area prefix k i j) (+ max-ending-here (calc-area prefix k i j))))
		   (setf max-so-far (funcall comparator max-so-far max-ending-here)))))))
    max-so-far))

(defun call-fun (f)
  (funcall f 0 0))

(call-fun #'+)
(defparameter mock-array (make-array '(5 5) :element-type 'fixnum))
(loop for i from 0 upto 4 do
  (loop for j from 0 upto 4 do
    (setf (aref mock-array i j) (if (= 0 (mod (+ i j) 2))
				    (+ i j)
				    (- (- i) j)))))
mock-array

(generic-kadane-2d mock-array #'min)

;; (defun teste (array)
;;   (declare (type (array fixnum 1) array))
;;   (loop for i from 0 upTo 8 do
;;        (format t "~a~%" (aref array i))))


;; (defparameter *array* (make-array '(9) :element-type 'fixnum :initial-element 10))
;; (defparameter *array2* (make-array '(9) :initial-element 999999999999999999999999))
;; (defparameter *array3* (make-array '(2 2) :element-type 'fixnum))
;; (loop for row from 0 upto 1 do
;;   (loop for col from 0 upto 1 do
;;     (setf (aref *array3* row col) (+ row col))))

;; (prefix-array *array3*)

;; (setf (aref *array* 0) 9)
;; (setf (aref *array* 0) 99999999999999999999999999999999999999999)
;; (teste *array*)

;; (teste *array2*)
