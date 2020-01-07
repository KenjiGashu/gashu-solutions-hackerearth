;; Sample code to perform I/O:

;; (setq name (read-line))                 ; Reading input from STDIN
;; (format t "Hi, ~F.~%" name)             ; Writing output to STDOUT

;; Warning: Printing unwanted or ill-formatted data to output will cause the test cases to fail

;; Write your code here
  (let* ((number-of-tests (read)))
    (declare (optimize (safety 0) (speed 3)))
    (dotimes (tests number-of-tests)
      (let* ((n (read))
	     (m (read))
	     (k (read))
	     (array (make-array (list n m) :element-type '(signed-byte 62)))
	     (cost-array-horizontal (make-array (list n m) :element-type '(signed-byte 62)))
	     (cost-array-vertical (make-array (list n m) :element-type '(signed-byte 62)))
	     (min most-positive-fixnum))
	(declare (type (array (signed-byte 62) *) array cost-array-horizontal cost-array-vertical))

	;;(format t "n: ~a m: ~a k: ~a~%" n m k)
	;;carrega array
	(dotimes (i n)
	  (dotimes (j m)
	    (the (signed-byte 62) (setf (aref array i j) (the fixnum (read))))))
	;;(format t "~a~%" array)

	;;calcula custo de arrays vertical e horizontal
	(when (<= k m)
	  (loop for i from 0 to (- n 2) do
	    (setf (aref cost-array-horizontal i 0) (the fixnum (+ (aref array i 0)
								  (aref array (+ i 1) 0))))
	    (loop for j from 1 to (1- m) do
	      (setf (aref cost-array-horizontal i j) (+ (aref array i j)
							(aref array (1+ i) j)
							(aref cost-array-horizontal i (1- j)))))))
	(when (<= k n)
	  (loop for j from 0 to (- m 2) do
	    (setf (aref cost-array-vertical 0 j) (the fixnum (+ (aref array 0 j )
								(aref array 0 (+ j 1)))))
	    (loop for i from 1 to (- n 1) do
	      (setf (aref cost-array-vertical i j) (+ (aref array i j)
						      (aref array i (+ j 1))
						      (aref cost-array-vertical (1- i) j))))))
	;;(format t "~a~%~a~%" cost-array-horizontal cost-array-vertical)
	(when (<= k m)
	  (loop  for i from 0 to (- n 2) do
	    (loop for j from (- k 1) to (1- m) do
	      (let ((sum (the fixnum (if (< (- j k) 0)
					 (aref cost-array-horizontal i j)
					 (- (aref cost-array-horizontal i j) (aref cost-array-horizontal i (- j k)))))))
		(when (< sum min)
		  (setf min sum))))))
	(when (<= k n)
	  (loop  for j from 0 to (- m 2) do
	    (loop for i from (- k 1) to (- n 1) do
	      (let ((sum (the fixnum (if (< (- i k) 0)
					 (aref cost-array-vertical i j)
					 (- (aref cost-array-vertical i j) (aref cost-array-vertical (- i k) j))))))
		(when (< sum min)
		  (setf min sum))))))
	(if (and (or (> k n)
		      (< m 2))
		 (or (> k m)
		      (< n 2)))
	    (format t "~a~%" -1)
	    (format t "~a~%" min))

	)))
