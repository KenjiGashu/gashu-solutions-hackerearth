(in-package :cl)
(defpackage :com.gashu.kenji.destination-cost
  (:use :cl))

(in-package :com.gashu.kenji.destination-cost)

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

(defclass arvore ()
  ((peso
    :accessor peso
    :initarg :peso
    :initform 0
    :documentation "peso das folhas")
   (esquerda
    :accessor esquerda
    :initform '()
    :initarg :esquerda)
   (direita
    :accessor direita
    :initform '()
    :initarg :direita)
   (valor
    :accessor valor
    :initform '()
    :initarg :valor)))

(defparameter *tree* '())
(defparameter *tree-instance* (make-instance 'arvore))
(setf (slot-value *tree-instance* 'valor) (cons 10 (slot-value *tree-instance* 'valor)))
(setf (slot-value *tree-instance* 'direita) (make-instance 'arvore :valor '(50) :esquerda (make-instance 'arvore :valor '(25))))
(defparameter *temp*
  (with-accessors ((direita direita) (valor valor) (peso peso)) *tree-instance*
    (let ((filho-direito direita))
      (with-accessors ((direita-direita direita) (direita-esquerda esquerda)) direita
      (let ((temp direita-esquerda))
	(setf direita-esquerda *tree-instance*)
	(setf direita temp)
	(format t "temp: ~a~%" temp)))
    filho-direito)))

(defparameter *temp* (balance-tree *tree-instance*))
(slot-value (slot-value *temp* 'esquerda) 'direita)
(slot-value *temp* 'direita)

(slot-value (slot-value *tree-instance* 'esquerda) 'direita)
(slot-value (slot-value *tree-instance* 'direita) 'esquerda)
(slot-value *tree-instance* 'direita)


(insert-tree *temp* 80)

(with-accessors ((esquerda esquerda) (direita direita) (valor valor)) *temp*
  (with-accessors ((eval valor)) esquerda
    (with-accessors ((direita-esquerda esquerda) (valorzim valor)) direita
	valor)
    ))

(with-accessors ((direita direita)) *tree-instance*
  (with-accessors ((direita-esquerda direita)) direita
    (setf direita-esquerda *tree-instance*)))


(with-accessors ((direita direita)) *tree-instance*
  (with-accessors ((direita-esquerda direita)) direita
    (slot-value direita-esquerda 'valor)))

(with-slots (direita esquerda valor) *tree-instance*
  (format t "~a~% ~a  ~%" valor (slot-value direita 'valor)))

(with-slots (direita esquerda valor) *tree-instance*
  direita)

(with-slots (direita esquerda valor) *temp*
  (format t "~a~% ~a  ~a ~%" valor (slot-value esquerda 'valor) (slot-value direita 'valor)))

(with-slots (direita esquerda valor) *temp*
  esquerda)



(defgeneric tree-contents (tree))
(defmethod tree-contents ((tree arvore))
  (slot-value tree 'valor))

(defgeneric tree-contents-value (tree))
(defmethod tree-contents-value ((tree arvore))
  (car (slot-value tree 'valor)))

(defgeneric balance-tree (tree))
(defmethod balance-tree ((tree arvore))
  (with-accessors ((esquerda esquerda) (direita direita) (peso peso) (valor valor)) tree
    (if (> (abs peso) 1)
	(with-accessors ((esquerda-direita direita) (esquerda-esquerda esquerda)) esquerda
	  (with-accessors ((ede esquerda) (edd direita)) esquerda-direita
	    (if (> (slot-value esquerda 'peso) 0)
		(let ((nova-raiz esquerda-direita))
		  (setf ede esquerda)
		  (setf esquerda-direita edd)
		  (setf esquerda edd)
		  (setf edd tree)
		  nova-raiz)
		(let ((nova-raiz esquerda)
		      (temp esquerda-direita))
		  (setf esquerda esquerda-direita)
		  (setf esquerda-direita tree)
		  nova-raiz))))
	
	(with-accessors ((direita-esquerda esquerda)) direita
	  (with-accessors ((ded direita) (dee esquerda)) direita-esquerda
	    (if (< (slot-value direita 'peso) 0)
		(let ((nova-raiz direita-esquerda))
		  (setf direita-esquerda ded)
		  (setf ded direita)
		  (setf direita dee)
		  (setf dee tree)
		  nova-raiz)
		(let ((nova-raiz direita)
		      (temp direita-esquerda))
		  (setf direita-esquerda tree)
		  (setf direita temp)
		  nova-raiz)))))))

(defgeneric insert-tree (tree val))
(defmethod insert-tree ((tree arvore) val)
  (with-accessors ((valor valor) (peso peso) (esquerda esquerda) (direita direita)) tree
    (cond ((> val (car valor)) (progn
				 (setf peso (1+ peso))
				 (setf direita (insert-tree direita val))))
	  ((< val (car valor)) (progn
				 (setf peso (1- peso))
				 (setf esquerda (insert-tree esquerda val))))
	      (t (setf valor (cons val valor))))
    (balance-tree tree)))
(defmethod insert-tree ((tree null) val)
  (make-instance 'arvore :valor (list val)))

(defun leaf-val (tree)
  "I don't think I use this function"
  (car tree))

(defun value-tree (tree)
  "returns the value stored on leaf.
1- If its a list, gets the first elt from the list
2- If its a elt, just return it"
  (let ((val (car tree)))
    (if (listp val)
	(car val)
	val)))

(defun left-branch (tree)
  "returns left branch"
  (car (cdr tree)))

(defun right-branch (tree)
  "returns the right branch"
  (cdr (cdr tree)))

(defun make-leaf (elt)
  "make leaf with elt, right and left branch will be nil"
  (cons elt (cons '() '())))

(defun make-tree (elt left right)
  "makes tree using elt, left and right"
  (cons elt (cons left right)))

(defun insert-tree (tree elt)
  "insert element into the right branch of tree
when the elt already exists on tree, add it to the leaf who has the same elt (doesn't make another leaf)"
  (if (null tree)
      (make-leaf elt)
      (cond ((> elt (value-tree tree)) (make-tree (leaf-val tree) (left-branch tree) (insert-tree (right-branch tree) elt)))
	    ((< elt (value-tree tree)) (make-tree (leaf-val tree) (insert-tree (left-branch tree) elt) (right-branch tree)))
	    (t (make-tree (insert-linkedlist (car tree) elt) (left-branch tree) (right-branch tree))))))

(defun insert-linkedlist (l elt)
  "adds elt to start o linked list (l)
   if l is an element, make it a list"
  (if (listp l)
      (cons elt l)
      (list l elt)))

(defun clear-tree ()
  "clear the global parameter *teste-tree*"
  (setf *teste-tree* '()))

(defun find-tree (tree val)
  "returns value val when it exist inside tree and nil otherwise.
Always returns two values:
1- elt
2- leaf where it was found"
  (if (null tree)
      (values '() '())
      (cond ((> val (value-tree tree)) (find-tree (right-branch tree) val))
	    ((< val (value-tree tree)) (find-tree (left-branch tree) val))
	    (t (values (value-tree tree) tree)))))

(defun pop-value (tree val)
  (if (null tree)
      (values '() '())
      (cond ((> val (value-tree tree)) (make-tree (leaf-val tree) (left-branch tree) (pop-value (right-branch tree) val)))
	    ((< val (value-tree tree)) (make-tree (leaf-val tree) (pop-value (left-branch tree) val) (right branch-tree)))
	    (t (values (value-tree tree) tree)))))
(cons 2 (list 2))
(list 2 2 )
(clear-tree )

(defparameter *teste-tree* '())
(setf *teste-tree* (insert-tree *teste-tree* 2))
(setf *teste-tree* (insert-tree *teste-tree* -5))
(setf *teste-tree* (insert-tree *teste-tree* 10))
(setf *teste-tree* (insert-tree *teste-tree* 15))
(setf *teste-tree* (insert-tree *teste-tree* 7))
(setf *teste-tree* (insert-tree *teste-tree* 3))
(setf *teste-tree* (insert-tree *teste-tree* -8))

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
