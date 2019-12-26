(defpackage :com.gashu.kenji.factorial
  (:use :cl))

(in-package :com.gashu.kenji.factorial)

;;auxiliar methods
(defparameter *dp* (make-array '(100000) :element-type :integer :initial-element -1))


(defun factorial (n)
  (let ((saved-result (aref *dp* n)))
	(when (< saved-result 0)
	  (if (= n 0)
	      (setf (aref *dp* n) 1)
	      (setf (aref *dp* n)
		    (* (factorial (1- n)) n))))
	;;(format t "~a~%" (aref *dp* n))
	(aref *dp* n)))

(defun factorial-optimized (n)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum n saved-results))
  (let ((saved-result (the fixnum (aref *dp* n))))
	(when (< saved-result 0)
	  (if (= n 0)
	      (the fixnum (setf (aref *dp* n) 1))
	      (setf (aref *dp* n)
		    (the fixnum (* (factorial-optimized (1- n)) n)))))
	;;(format t "~a~%" (aref *dp* n))
	(the fixnum (aref *dp* n))))
(defun create-test-file ()
  (with-open-file (out "factorial-dp.data" :direction :output)
    (dotimes (i 100000)
      (format out "~a~%" i))))

;;(create-test-file)

(defun plus (a b)
  (+ a b))

(defun plus-opt (a b)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum a b))
  (the fixnum (+ a b)))

(disassemble 'plus)

(disassemble 'plus-opt)

(plus-opt 9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999)

;;read inputs and solve
(defparameter *test-cases* (parse-integer (read-line)))

(time
 (with-open-file (in "factorial-dp.data" :direction :input)
   (dotimes (i *test-cases*)
     (declare (type fixnum in-number))
     (let ((in-number (parse-integer (read-line in))))
       ;;(format t "~a~%" in-number)
       (format t "~a~%" (factorial-optimized in-number))))))


