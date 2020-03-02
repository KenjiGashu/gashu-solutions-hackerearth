(in-package :cl)

(defpackage :com.gashu.digit-string
  (:use :cl))

(in-package :com.gashu.digit-string)

(defun get-size (n i size)
  (if (> (+ i size) n)
      n
      (+ i size)))

(defun partition (str n k)
  (let ((ok 1)
	(resp 1))
    (loop for size = 1 then (1+ size)
	  while (and (<= size n) (> ok 0))
	  do
	     (setf resp size)
	     (loop for i = 0 then (+ i size)
		   while (< i n)
		   do (when (> (parse-integer (subseq str i (get-size n i size))) k)
			(setf resp (1- size))
			(setf ok 0))))
    resp))

(partition "34212" 5 6)
(partition "11" 2 21)

(defun solve ()
  (let ((number-tests (read))
	(n 0)
	(k 0)
	(str 0))
    (loop for i from 1 to number-tests do
      (progn
	(setf n (read))
	(setf k (read))
	(setf str (write-to-string (read)))
	(format t "~a~%" (partition str n k))))))

(defun solve-from-file ()
  (with-open-file (in "digit-strings.data2" :direction :input)
    (let ((number-tests (read in))
	(n 0)
	(k 0)
	(str 0))
    (loop for i from 1 to number-tests do
      (progn
	(setf n (read in))
	(setf k (read in))
	(setf str (write-to-string (read in)))
	(format t "~a~%" (partition str n k)))))))

(solve-from-file)
