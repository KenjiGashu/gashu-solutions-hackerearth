(in-package :cl)

(defpackage :com.gashu.alice-and-bob-buy-crackers
  (:use :cl))

(in-package :com.gashu.alice-and-bob-buy-crackers)

(defun solve (n c sum pos)
  (if (= n pos)
      (if (and (= 0 (mod sum 2)) (not (= sum 0)))
	  (progn (format t "sum:~a~% " sum)
		 1)
	  0)
      (+ (solve n c (+ sum (aref c pos)) (1+ pos))
	 (solve n c sum (1+ pos)))))

(defun setup ()
  (let* ((n (read))
	(c (make-array (list n))))
    (loop for i from 0 to (1- n) do
      (setf (aref c i) (read)))
    (solve n c 0 0)))

(setup)
