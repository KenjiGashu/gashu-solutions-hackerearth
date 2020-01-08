(in-package :cl)

(defpackage :kadane-1d
  (:use :cl))

(in-package :kadane-1d)

(defun max-subarray (array)
  (labels ((local-max (array index current-sum best-sum)
	   (let ((len (array-dimension array 0)))
	     (if (< index len)
		 (let* ((x (aref array index))
			(current (+ x current-sum)))
		   (if (< current 0)
		       (local-max array (1+ index) 0 (max best-sum current))
		       (local-max array (1+ index) current (max best-sum current))))
		 best-sum))))
    (funcall #'local-max array 0 0 0)))

(defun max-subarray2 (array)
  (labels ((local-max (array index current-sum best-sum)
	   (let ((len (array-dimension array 0)))
	     (if (< index len)
		 (let* ((x (aref array index))
			(current (+ x current-sum))
			(max-here (max current x)))
		       (local-max array (1+ index) max-here (max best-sum max-here)))
		 best-sum))))
    (funcall #'local-max array 0 0 0)))


(defparameter *teste* #(1 -2 5 -7 10 50 -30))
(defparameter *teste2* #(-2 -3 4 -1 -2 1 5 -3))

(max-subarray2 *teste*)
(max-subarray2 *teste2*)

(flet ((dummy-function () 'shadow)) 
  (funcall #'dummy-function))
