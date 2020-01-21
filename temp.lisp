;; Debugging Lisp

(in-package :cl-user)

(defun fib (n)
  (if (<= 0 n 1)
      n 
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 10)

(defclass point ()
  ((x :accessor point-x :initarg :x :initform 0)
   (y :accessor point-y :initarg :y :initform 0)))

(make-instance 'point :x 10 :y 20)

(defun sum (xs &optional (acc 0))
  (if (null xs)
      acc
      (sum (cdr xs) (+ (car xs) acc))))


(sum '(1 2 3 4 5))


;;Part 3
(defparameter *location* (make-instance 'point :x 100 :y 100))

(defmethod update-instance-for-redefined-class :before
     ((pos point) added deleted plist &key)
  (let ((x (getf plist 'x))
        (y (getf plist 'y)))
    (setf (point-rho pos) (sqrt (+ (* x x) (* y y)))
          (point-theta pos) (atan y x))))
  
(defclass point ()
  ((rho :initform 0 :accessor point-rho)
   (theta :initform 0 :accessor point-theta)))
  
(defmethod point-x ((pos point))
  (with-slots (rho theta) pos (* rho (cos theta))))
  
(defmethod point-y ((pos point))
  (with-slots (rho theta) pos (* rho (sin theta))))

;; Part 4
(defun read-file (file)
  (with-open-file (in file :direction :input)
    (loop for line = (read-line in nil in)
          until (eq in line)
          collect (parse-integer line))))
  
(defun read-files (files)
  (loop for file in files
        collect (read-file file)))

(defun read-file (file)
  (with-open-file (in file :direction :input)
    (loop for line = (read-line in nil in)
          until (eq in line)
          when (handler-case (parse-integer line)
                 ;; C is the name being used to
                 ;; refer to the exception object.
                 (error (c)
                   (declare (ignore c))
                   nil))
          collect it)))
  
(read-files *files*)


(defvar *malformed-value* nil)
  
(defun read-file (file)
  (with-open-file (in file :direction :input)
    (loop for line = (read-line in nil in)
          until (eq in line)
          when (handler-case (parse-integer line)
                 (error (c)
                   (declare (ignore c))
                   *malformed-value*))
          collect it)))
  
(let ((*malformed-value* :malformed))
  (read-files *files*))

(defun read-files (files)
  (loop for file in files
        when (handler-case (read-file file)
               (error (c)
                 (declare (ignore c))
                 nil))
        collect it))
  
(read-files *files*)

(defparameter *files* (list "data1" "data2"))

(scan "(afafafa" "afafaf")
