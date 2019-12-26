(in-package :cl)

(defpackage :com.gashu.kenji.binary-search
  (:use :cl))

(in-package :com.gashu.kenji.binary-search)

(defun binary-search (array n start end)
  (format t "start: ~a end: ~a ~%" start end)
  (if (>= start end)
      (if (= (aref array start) n)
	  start
	  -1)
      (let ((half (aref array (floor (/ (+ end start) 2)))))
	(cond ((= half n) (progn (format t "value: ~a position: ~a~%" half (floor (/ (+ end start) 2))))) 
	      ((< n half) (binary-search array n start (1- half)))
	      ((> n half) (binary-search array n (1+ half) end))))))

(defparameter *array-size* (parse-integer (read-line)))
(defparameter *array* (make-array (list *array-size*) :element-type :integer))
(dotimes (i *array-size*)
  (setf (aref *array* i) (parse-integer (read))))

(defparameter *tests* (parse-integer (read-line)))

(dotimes (i *tests*)
  (binary-search *array* (parse-integer (read-line)) 0 50))
;; (defparameter *array* (make-array (list 50) :element-type :integer))
;; (dotimes (i 50)
;;   (setf (aref *array* i) i))


;; (binary-search *array* -1 0 50)
;; (binary-search *array* 51 0 49)
