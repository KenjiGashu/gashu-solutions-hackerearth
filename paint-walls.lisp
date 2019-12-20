;; doesnt pass most of the test cases because of timeout

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
     :then (position-if-not delimiterp string :start (1+ end))
     :for end = (and beg (position-if delimiterp string :start beg))
     :when beg :collect (subseq string beg end)
     :while end))


(defparameter test-filename "paint-walls.data")

(defparameter test-num (parse-integer (read-line)))

(defparameter mock-array (make-array '(3 3) :element-type :integer :initial-element 3))
(defparameter mock-array2 (make-array '(1 1) :element-type :integer :initial-element 1))
(defparameter mock-array3 (make-array '(2 2) :element-type :integer :initial-element 1))
(defparameter mock-array4 (make-array '(5 5) :element-type :integer))
(defparameter mock-array5 (make-array '(2 5 5) :element-type :integer :initial-element 3) )

(with-open-file (out "teste-paint-walls.data" :direction :output)
  (write mock-array5 :stream out))
(defparameter teste
  (with-open-file (in "teste-paint-walls.data" :direction :input)
    (read in)))


;; (loop for k from 0 to 1 do
;;      (loop for i from 0 to 4 do
;; 	  (loop for j from 0 to 4 do (setf (aref mock-array5 k i j) (* (1+ i) (1+ j))))))

;; (defun kasdksa (costs saved min i j width height))


;; mock-array4 

(defun sum-cost (array start-row start-col height width)
  (format t "start-row: ~a start-col: ~a height: ~a width: ~a~%" start-row start-col height width)
  (let ((sum-total 0))
    (dotimes (i height)
      (dotimes (j width)
	(progn
	  (setf sum-total (+ sum-total (aref array (+ i start-row) (+ j start-col))))
	  ;;(format t "elt: ~a sum: ~a~%" (aref array i j) sum-total)
	  )))
    (format t "retorno sum-cost: ~a~%" sum-total)
    sum-total))

;; (defun solve-recursive (array k)
;;   (flet recur (array )))

(defparameter *quadrantes* (make-hash-table))

;; TODO
(defun calcula-quadrante (id)
  (destructuring-bind (a b c d) id
    (loop from a upto )))


;; TODO
(defun get-quadrante (id)
  (let ((q (get-hash (list id) *quadrantes*)))
    (unless q
      (setf (get-hash '(id) *quadrantes*) (calcula-quadrante id)))))

(defun cost-0-0 (costs width height tilted?)
  (loop for i from 0 to 1 summing
       (loop for j from 0 to width summing
	    (loop for k from 0 to height summing
		 (if (> 0 i)
		     (aref costs 0 i j)
		     (aref costs 1 j i))))))
(cost-0-0 mock-array5 2 2 1)
(aref mock-array5 0 0 0)

(defun getcost (costs saved-costs i j width height)
  (let ((saved (if (> width height)
		   (aref saved-costs 0 i (1- j))
		   (aref saved-costs 1 (1- i) j))))
    (if (< 0 saved)
	(if (> width height)
	    (setf (aref saved-costs 0 i j) (+
					    (getcost costs saved-costs i (1- j) width height)
					    (- (aref saved-costs 0 i (- j witdh 1)))
					    (- (aref saved-costs 0 (1- i) (- j width 1)))
					    (aref saved-costs 0 i (1+ j))
					    (aref saved-costs 0 (1- i) (1+ j))))
	    (setf (aref saved-costs 1 i j) (getcost costs saved-costs (1- i) j width height)))
	saved)))


(defun step-0 (costs width height)
  "returns a array of already calculated costs and an answer array with
base case"
  (let ((answer-array (make-array (list 2 (max (- n 2) (- m 2))) :element-type :integer))
	(calculated-costs (cost-array (make-array (list 2 (- n 2) (- m 2)) :element-type :integer :initial-element -1))))
    ))

;;solution
;;dotimes (i test-num)
;; reads n m k and create array of dimensions N x M
(dotimes (cases test-num)
  (let* ((values (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space))))
	 (n (parse-integer (nth 0 values)))
	 (m (parse-integer (nth 1 values)))
	 (k (parse-integer (nth 2 values)))
	 (array (make-array (list n m) :element-type :integer))
	 (cost-array-horizontal (make-array (list n m) :element-type :integer))
	 (cost-array-vertical (make-array (list n m) :element-type :integer))
	 (min 999999999999999999)
	 )
    (declare (optimize speed))
    ;;(format t "n: ~a m: ~a k: ~a~%" n m k)
    ;;populate array
    (dotimes (j n)
      (let* ((row (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
	(dotimes (z m)
	  (setf (aref array j z) (parse-integer (nth z row))))))
    
    (format t "array: ~a~%" array)

    (setf (aref cost-array-horizontal 0 0) (+ (aref array 0 0)
					      (aref array 1 0)))
    (setf (aref cost-array-vertical 0 0) (+ (aref array 0 0 )
					    (aref array 0 1)))
    (when (<= k m)
      (loop for i from 0 to (- n 2) do
	   (loop for j from 1 to (1- m) do
		(setf (aref cost-array-horizontal i j) (+ (aref array i j)
							  (aref array (1+ i) j)
							  (aref cost-array-horizontal i (1- j)))))))
    (format t "array custo horizontal: ~a~%" cost-array-horizontal)

    (when (<= k n)
      (loop for j from 0 to (- m 2) do
	   (loop for i from 1 to (- n 1) do
		(setf (aref cost-array-vertical i j) (+ (aref array i j)
							(aref array i (+ j 1))
							(aref cost-array-vertical (1- i) j))))))
    (format t "array custo vertical: ~a~%" cost-array-vertical)
    (when (<= k m)
      (loop  for i from 0 to (- n 2) do
	   (loop for j from (- k 1) to (1- m) do
		(let ((sum (if (<= 0 (- j k))
			       (aref cost-array-horizontal i j)
			       (- (aref cost-array-horizontal i j) (aref cost-array-horizontal i (- j k))))))
		  (when (< sum min)
		    (setf min sum))))))
    (when (<= k n)
      (loop  for j from 0 to (- m 2) do
	   (loop for i from (- k 1) to (- n 1) do
		(let ((sum (if (<= 0 (- i k))
			       (aref cost-array-vertical i j)
			       (- (aref cost-array-vertical i j) (aref cost-array-vertical (- i k) j)))))
		  (when (< sum min)
		    (setf min sum))))))
    (when (and (> k n)
	       (> k m))
      (format t "~a~%" -1))))

;;to test with slime using paint-walls.data as input
(with-open-file (in test-filename :direction :input)
  (defparameter test-num (parse-integer (read-line in)))
  (dotimes (i test-num)
    ;; reads n m k and create array of dimensions N x M
    (let* ((values (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space))))
	   (n (parse-integer (nth 0 values)))
	   (m (parse-integer (nth 1 values)))
	   (k (parse-integer (nth 2 values)))
	   (array (make-array `(,n ,m) :element-type :integer))
	   (cost-array-horizontal (make-array (list n m) :element-type :integer))
	   (cost-array-vertical (make-array (list n m) :element-type :integer))
	   (min 999999999999999999))
      ;;(format t "n: ~a m: ~a k: ~a~%" n m k)
      ;;populate array
      (dotimes (j n)
	(let* ((row (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
	  (dotimes (z m)
	    (setf (aref array j z) (parse-integer (nth z row))))))

      (format t "array: ~a~%" array)

      (loop for i from 0 to (- n 2) do
	   (setf (aref cost-array-horizontal i 0) (+ (aref array i 0)
						     (aref array (+ i 1) 0))))
      
      (loop for j from 0 to (- m 2) do
	   (setf (aref cost-array-vertical 0 j) (+ (aref array 0 j )
						   (aref array 0 (+ j 1)))))
      
      
      (when (<= k m)
	(loop for i from 0 to (- n 2) do
	     (loop for j from 1 to (1- m) do
		  (setf (aref cost-array-horizontal i j) (+ (aref array i j)
							    (aref array (1+ i) j)
							    (aref cost-array-horizontal i (1- j)))))))
      (format t "array custo horizontal: ~a~%" cost-array-horizontal)

      (when (<= k n)
	(loop for j from 0 to (- m 2) do
	     (loop for i from 1 to (- n 1) do
		  (setf (aref cost-array-vertical i j) (+ (aref array i j)
							  (aref array i (+ j 1))
							  (aref cost-array-vertical (1- i) j))))))
      (format t "array custo vertical: ~a~%" cost-array-vertical)
      (when (<= k m)
	(loop  for i from 0 to (- n 2) do
	     (loop for j from (- k 1) to (1- m) do
		  (let ((sum (if (< (- j k) 0)
				 (aref cost-array-horizontal i j)
				 (- (aref cost-array-horizontal i j) (aref cost-array-horizontal i (- j k))))))
		    (when (< sum min)
		      (setf min sum))))))
      (when (<= k n)
	(loop  for j from 0 to (- m 2) do
	     (loop for i from (- k 1) to (- n 1) do
		  (let ((sum (if (< (- i k) 0)
				 (aref cost-array-vertical i j)
				 (- (aref cost-array-vertical i j) (aref cost-array-vertical (- i k) j)))))
		    (when (< sum min)
		      (setf min sum))))))
      (if (and (> k n)
	       (> k m))
	  (format t "~a~%" -1)
	  (format t "~a~%" min)))))



;; unoptimal solution (it calculates every possible array position)

;; (defun solve (array k)
;;   (let ((answer nil)
;; 	(n (nth 0 (array-dimensions array)))
;; 	(m (nth 1 (array-dimensions array))))
;;     (format t "solve call -> n: ~a m: ~a k: ~a~%" n m k)
;;     ;; try to paint logo 2 x K
;;     (when (and (<= 2 n)
;; 	       (<= k m))
;;       (let ((col-tries (- m k))
;; 	    (row-tries (- n 2)))
;; 	(format t "col-tries: ~a  row-tries ~a~%" col-tries row-tries)
;; 	(dotimes (row (1+ row-tries))
;; 	  (dotimes (col (1+ col-tries))
;; 	    (let ((sumcost (sum-cost array row col 2 k)))
;; 	      (format t "sumcost: ~a answer: ~a~%" sumcost answer )
;; 	      (when (or (null answer)
;; 			(> answer sumcost))
;; 		(format t "atualiza answer~%")
;; 		(setf answer sumcost)))))))
;;     ;;try to paint logo K x 2
;;     (when (and (<= k n)
;; 	       (<= 2 m))
;;       (let ((col-tries (- m 2))
;; 	    (row-tries (- n k)))
;; 	(format t "col-tries: ~a  row-tries ~a~%" col-tries row-tries)
;; 	(dotimes (row (1+ row-tries))
;; 	  (dotimes (col (1+ col-tries))
;; 	    (let ((sumcost (sum-cost array row col k 2)))
;; 	      (format t "sumcost: ~a answer: ~a~%" sumcost answer)
;; 	      (when (or (null answer)
;; 			(> answer sumcost))
;; 		(format t "atualiza answer~%")
;; 		(setf answer sumcost)))))))
;;     (if (null answer)
;; 	(format t "~a~%" -1)
;; 	(format t "~a~%" answer))))



;;random things

;; (defun test ()
;;   "just testing local defun"
;;   (labels ((temp (n) (* n 6)))
;;     (temp 7)))

;; (defun recursive-times (k n)
;;   "i copy pasted an example of local defun"
;;    (labels ((temp (n) 
;;               (if (zerop n) 0 (+ k (temp (1- n))))))
;;      (temp n)))

(with-open-file (in "output-#7.txt" :direction :input)
  (read in)
  (read in)
  (read in)
  (read in)
  )

(eval (read-from-string "(+ 3 3)")) 
(read-from-string "(+ 3 3)")
(time (with-open-file (in "output-#7.txt" :direction :input)
	(defparameter test-num (parse-integer (nth 1 (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space))))))
	(format t "test-num: ~a~%" test-num)
	(dotimes (i test-num)
	  ;; reads n m k and create array of dimensions N x M
	  (let* ((values (progn
			   (let ((splatterson (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
			     (when (> 2 (length splatterson))
			       (setf splatterson (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
			     splatterson)))
		 (n (progn
		      
		      (parse-integer (nth 1 values))))
		 (m (parse-integer (nth 3 values)))
		 (k (parse-integer (nth 5 values)))
		 (array (make-array (list n m) :element-type :integer))
		 (cost-array-horizontal (make-array (list n m) :element-type :integer))
		 (cost-array-vertical (make-array (list n m) :element-type :integer))
		 (min 999999999999999999))
	    (declare (optimize (speed 3)))
	    (format t "n: ~a m: ~a k: ~a~%" n m k)
	    ;;populate array
	    ;; (dotimes (j n)
	    ;; 	(let* ((row (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
	    ;; 	  (dotimes (z m)
	    ;; 	    (setf (aref array j z) (parse-integer (nth z row))))))
	    (format t "read: ~a~%" (read in))
	    (setf array (read in))
	    ;;(format t "~a~%" array)

	    (loop for i from 0 to (- n 2) do
		 (setf (aref cost-array-horizontal i 0) (+ (aref array i 0)
							   (aref array (+ i 1) 0))))
	    
	    (loop for j from 0 to (- m 2) do
		 (setf (aref cost-array-vertical 0 j) (+ (aref array 0 j )
							 (aref array 0 (+ j 1)))))
	    
	    
	    (when (<= k m)
	      (loop for i from 0 to (- n 2) do
		   (loop for j from 1 to (1- m) do
			(setf (aref cost-array-horizontal i j) (+ (aref array i j)
								  (aref array (1+ i) j)
								  (aref cost-array-horizontal i (1- j)))))))
	    ;;(format t "array custo horizontal: ~a~%" cost-array-horizontal)

	    (when (<= k n)
	      (loop for j from 0 to (- m 2) do
		   (loop for i from 1 to (- n 1) do
			(setf (aref cost-array-vertical i j) (+ (aref array i j)
								(aref array i (+ j 1))
								(aref cost-array-vertical (1- i) j))))))
	    ;;(format t "array custo vertical: ~a~%" cost-array-vertical)
	    (when (<= k m)
	      (loop  for i from 0 to (- n 2) do
		   (loop for j from (- k 1) to (1- m) do
			(let ((sum (if (< (- j k) 0)
				       (aref cost-array-horizontal i j)
				       (- (aref cost-array-horizontal i j) (aref cost-array-horizontal i (- j k))))))
			  (when (< sum min)
			    (setf min sum))))))
	    (when (<= k n)
	      (loop  for j from 0 to (- m 2) do
		   (loop for i from (- k 1) to (- n 1) do
			(let ((sum (if (< (- i k) 0)
				       (aref cost-array-vertical i j)
				       (- (aref cost-array-vertical i j) (aref cost-array-vertical (- i k) j)))))
			  (when (< sum min)
			    (setf min sum))))))
	    (if (and (> k n)
		     (> k m))
		(format t "~a~%" -1)
		(format t "~a~%" min))))))


(time (with-open-file (in test-filename :direction :input)
	(defparameter test-num (parse-integer (nth 1 (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space))))))
	(dotimes (i test-num)
	  ;; reads n m k and create array of dimensions N x M
	  (let* ((values (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space))))
		 (n (parse-integer (nth 1 values)))
		 (m (parse-integer (nth 3 values)))
		 (k (parse-integer (nth 5 values)))
		 (array (make-array (list n m) :element-type :integer))
		 (cost-array-horizontal (make-array (list n m) :element-type :integer))
		 (cost-array-vertical (make-array (list n m) :element-type :integer))
		 (min 999999999999999999))
	    ;;(format t "n: ~a m: ~a k: ~a~%" n m k)
	    ;;populate array
	    ;; (dotimes (j n)
	    ;; 	(let* ((row (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
	    ;; 	  (dotimes (z m)
	    ;; 	    (setf (aref array j z) (parse-integer (nth z row))))))
	    (read in)
	    (setf array (read in))

	    (format t "array: ~a~%" array)

	    (loop for i from 0 to (- n 2) do
		 (setf (aref cost-array-horizontal i 0) (+ (aref array i 0)
							   (aref array (+ i 1) 0))))
	    
	    (loop for j from 0 to (- m 2) do
		 (setf (aref cost-array-vertical 0 j) (+ (aref array 0 j )
							 (aref array 0 (+ j 1)))))
	    
	    
	    (when (<= k m)
	      (loop for i from 0 to (- n 2) do
		   (loop for j from 1 to (1- m) do
			(setf (aref cost-array-horizontal i j) (+ (aref array i j)
								  (aref array (1+ i) j)
								  (aref cost-array-horizontal i (1- j)))))))
	    (format t "array custo horizontal: ~a~%" cost-array-horizontal)

	    (when (<= k n)
	      (loop for j from 0 to (- m 2) do
		   (loop for i from 1 to (- n 1) do
			(setf (aref cost-array-vertical i j) (+ (aref array i j)
								(aref array i (+ j 1))
								(aref cost-array-vertical (1- i) j))))))
	    (format t "array custo vertical: ~a~%" cost-array-vertical)
	    (when (<= k m)
	      (loop  for i from 0 to (- n 2) do
		   (loop for j from (- k 1) to (1- m) do
			(let ((sum (if (< (- j k) 0)
				       (aref cost-array-horizontal i j)
				       (- (aref cost-array-horizontal i j) (aref cost-array-horizontal i (- j k))))))
			  (when (< sum min)
			    (setf min sum))))))
	    (when (<= k n)
	      (loop  for j from 0 to (- m 2) do
		   (loop for i from (- k 1) to (- n 1) do
			(let ((sum (if (< (- i k) 0)
				       (aref cost-array-vertical i j)
				       (- (aref cost-array-vertical i j) (aref cost-array-vertical (- i k) j)))))
			  (when (< sum min)
			    (setf min sum))))))
	    (if (and (> k n)
		     (> k m))
		(format t "~a~%" -1)
		(format t "~a~%" min))))))
