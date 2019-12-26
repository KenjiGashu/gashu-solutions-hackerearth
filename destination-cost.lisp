(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
	  :then (position-if-not delimiterp string :start (1+ end))
	:for end = (and beg (position-if delimiterp string :start beg))
	:when beg :collect (subseq string beg end)
	  :while end))


(defparameter test-filename "destination-cost.data")

(defparameter cities (parse-integer (read-line)))

(let* ((cars-cost (mapcar #'parse-integer (my-split (read-line) :delimiterp (lambda (c) (char= c #\Space)))))))

(defun solve (cities cars-cost bus-cost step answer total-cost)
  (if (< step cities)
      (min (solve cities cars-cost bus-cost (1+ step) (cons (cons (nth step cars-cost) 'car) answer) (+ (nth step cars-cost) total-cost))
	   (solve cities cars-cost bus-cost (1+ step) (cons (cons (nth step bus-cost) 'car) answer) (+ (nth step bus-cost) total-cost)))
      total-cost))

(defun solve2 (cities cars-cost bus-cost step answer total-cost)
  (if (< step cities)
      (solve2 cities
	      cars-cost
	      bus-cost
	      (1+ step)
	      (cons (min (aref cars-cost step)
			 (aref bus-cost step))
		    answer)
	      (+ total-cost (min (aref cars-cost step)
				 (aref bus-cost step))))
      total-cost))

(defparameter *tree* '())

(defun leaf-val (tree)
  (car tree))

(defun value-tree (tree)
  (let ((val (car tree)))
    (if (listp val)
	(car val)
	val)))

(defun left-branch (tree)
  (car (cdr tree)))

(defun right-branch (tree)
  (cdr (cdr tree)))

(defun make-leaf (elt)
  (cons elt (cons '() '())))

(defun make-tree (elt left right)
  (cons elt (cons left right)))

(defun insert-tree (tree elt)
  (if (null tree)
      (make-leaf elt)
      (cond ((> elt (value-tree tree)) (make-tree (value-tree tree) (left-branch tree) (insert-tree (right-branch tree) elt)))
	    ((< elt (value-tree tree)) (make-tree (value-tree tree) (insert-tree (left-branch tree) elt) (right-branch tree)))
	    (t (make-tree (insert-linkedlist (car tree) elt) (left-branch tree) (right-branch tree))))))

(defun insert-linkedlist (l elt)
  (if (listp l)
      (cons elt l)
      (list l elt)))

(defun clear-tree ()
  (setf *teste-tree* '()))

(defun find-tree (tree val)
  (if (null tree)
      '()
      (cond ((> val (value-tree tree)) (find-tree (right-branch tree) val))
	    ((< val (value-tree tree)) (find-tree (left-branch tree) val))
	    (t (values (value-tree tree) tree)))))

(defun pop-value (tree val))
(cons 2 (list 2))
(list 2 2 )
(clear-tree )

(defparameter *teste-tree* '())
(setf *teste-tree* (insert-tree *teste-tree* 2))
(setf *teste-tree* (insert-tree *teste-tree* -5))

(make-tree 2 '() '())

(value-tree (make-tree 2 '() '()))
(left-branch (make-tree 2 '() '()))
(right-branch (make-tree 2 '() '()))

(insert-tree (make-tree 2 '() '()) 3)

(right-branch (insert-tree (make-tree 2 '() '()) 3))

(time (with-open-file (in "destination-cost3.data" :direction :input)
	 (let* ((cities (parse-integer (read-line in)))
		;;(cars-cost (mapcar #'parse-integer (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
		;;(bus-cost (mapcar #'parse-integer (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
		(cars-cost (make-array (list cities) :element-type :integer))
		(bus-cost (make-array (list cities) :element-type :integer))
		(min nil))
	   (format t "number of cities: ~a~%" cities)
	   (dotimes (i cities)
	     (let ((number (read in)))
	       ;;(format t "read-number: ~a~%" number)
	       (setf (aref cars-cost i) number)))
	   (dotimes (i cities)
	     (setf (aref bus-cost i) (read in)))
					;(format t "cities: ~a cars-cost: ~a bus-cost: ~a~%" cities cars-cost bus-cost)
	   ;;(format t "cars: ~a~%~%" cars-cost)
	   ;;(format t "bus: ~a~%~%" bus-cost)
	   (format t "~a~%" (solve2 cities cars-cost bus-cost 0 '() 0))
	   )))
	   ;;)))



(with-open-file (in "destination-cost2.data" :direction :input)
  (dotimes (i 20)
    (format t "~a~%" (read in))))
