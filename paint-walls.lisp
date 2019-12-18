;; Sample code to perform I/O:

;; (setq name (read-line))                 ; Reading input from STDIN
;; (format t "Hi, ~F.~%" name)             ; Writing output to STDOUT

;; Warning: Printing unwanted or ill-formatted data to output will cause the test cases to fail

;; Write your code here
;;(defun delimiterp (c) (char= c #\Space))

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

(defun test ()
  "just testing local defun"
  (labels ((temp (n) (* n 6)))
    (temp 7)))

(defun recursive-times (k n)
  "i copy pasted an example of local defun"
   (labels ((temp (n) 
              (if (zerop n) 0 (+ k (temp (1- n))))))
     (temp n)))

(defun solve (array k)
  (let ((answer nil)
	(n (nth 0 (array-dimensions array)))
	(m (nth 1 (array-dimensions array))))
    (format t "solve call -> n: ~a m: ~a k: ~a~%" n m k)
    ;; try to paint logo 2 x K
    (when (and (<= 2 n)
	       (<= k m))
      (let ((col-tries (- m k))
	    (row-tries (- n 2)))
	(format t "col-tries: ~a  row-tries ~a~%" col-tries row-tries)
	(dotimes (row (1+ row-tries))
	  (dotimes (col (1+ col-tries))
	    (let ((sumcost (sum-cost array row col 2 k)))
	      (format t "sumcost: ~a answer: ~a~%" sumcost answer )
	      (when (or (null answer)
			(> answer sumcost))
		(format t "atualiza answer~%")
		(setf answer sumcost)))))))
    ;;try to paint logo K x 2
    (when (and (<= k n)
	       (<= 2 m))
      (let ((col-tries (- m 2))
	    (row-tries (- n k)))
	(format t "col-tries: ~a  row-tries ~a~%" col-tries row-tries)
	(dotimes (row (1+ row-tries))
	  (dotimes (col (1+ col-tries))
	    (let ((sumcost (sum-cost array row col k 2)))
	      (format t "sumcost: ~a answer: ~a~%" sumcost answer)
	      (when (or (null answer)
			(> answer sumcost))
		(format t "atualiza answer~%")
		(setf answer sumcost)))))))
    (if (null answer)
	(format t "~a~%" -1)
	(format t "~a~%" answer))))

;;solution
;;dotimes (i test-num)
;; reads n m k and create array of dimensions N x M
(dotimes (cases test-num)
  (let* ((values (my-split (read-line) :delimiterp (lambda (c) (char= c #\Space))))
       (n (parse-integer (nth 0 values)))
       (m (parse-integer (nth 1 values)))
       (k (parse-integer (nth 2 values)))
	 (array (progn (format t "valores n: ~a m: ~a k: ~a~%" n m k)
		       (make-array `(,n ,m) :element-type :integer))))
  ;;populate array
  (dotimes (j n)
    (let* ((row (my-split (read-line) :delimiterp (lambda (c) (char= c #\Space)))))
      (dotimes (z m)
	(setf (aref array j z) (parse-integer (nth z row))))))
  (solve array k)))

;;to test with slime using paint-walls.data as input
(with-open-file (in test-filename :direction :input)
  (defparameter test-num (parse-integer (read-line in)))
  (dotimes (i test-num)
    ;; reads n m k and create array of dimensions N x M
    (let* ((values (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space))))
	   (n (parse-integer (nth 0 values)))
	   (m (parse-integer (nth 1 values)))
	   (k (parse-integer (nth 2 values)))
	   (array (make-array `(,n ,m) :element-type :integer)))
      ;;(format t "n: ~a m: ~a k: ~a~%" n m k)
      ;;populate array
      (dotimes (j n)
	(let* ((row (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
	  (dotimes (z m)
	    (setf (aref array j z) (parse-integer (nth z row))))))
      (solve array k))))
