(in-package :cl)
(defpackage :com.gashu.kenji.destination-cost
  (:use :cl))

(in-package :com.gashu.kenji.destination-cost)

(declaim (optimize (debug 3)))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
	  :then (position-if-not delimiterp string :start (1+ end))
	:for end = (and beg (position-if delimiterp string :start beg))
	:when beg :collect (subseq string beg end)
	  :while end))


(defparameter test-filename "destination-cost.data")

(defparameter cities (parse-integer (read-line)))


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

(defmethod print-object ((obj arvore) out)
  (print-unreadable-object (obj out :type t)
    (format out "V:~a W:~a L:~:[NIL~;Branch~] R:~:[NIL~;Branch~]" (valor obj) (peso obj) (esquerda obj) (direita obj))))

(defgeneric tree-contents (tree))
(defmethod tree-contents ((tree arvore))
  (slot-value tree 'valor))

(defgeneric tree-contents-value (tree))
(defmethod tree-contents-value ((tree arvore))
  (car (slot-value tree 'valor)))

(defgeneric balance-tree (tree))
(defmethod balance-tree ((tree arvore))
  (with-accessors ((esquerda esquerda) (direita direita) (peso peso) (valor valor)) tree
    (cond ((> peso 1)
	   (with-accessors ((esquerda-direita direita) (esquerda-esquerda esquerda)) esquerda
	     (with-accessors ((ede esquerda) (edd direita)) esquerda-direita
	       (if (< (peso esquerda) 0)
		   (let ((nova-raiz esquerda-direita))
		     (break)
		     (setf ede esquerda)
		     (setf esquerda-direita edd)
		     (setf esquerda edd)
		     (setf edd tree)
		     nova-raiz)
		   (let ((nova-raiz esquerda)
			 (temp esquerda-direita))
		     (setf esquerda esquerda-direita)
		     (setf esquerda-direita tree)
		     nova-raiz)))))
	  
	  ((< peso -1)
	   (with-accessors ((direita-esquerda esquerda)) direita
	     (with-accessors ((ded direita) (dee esquerda)) direita-esquerda
	       (if (> (slot-value direita 'peso) 0)
		   (let ((nova-raiz direita-esquerda))
		     (break)
		     (setf direita-esquerda ded)
		     (setf ded direita)
		     (setf direita dee)
		     (setf dee tree)
		     nova-raiz)
		   (let ((nova-raiz direita)
			 (temp direita-esquerda))
		     (setf direita-esquerda tree)
		     (setf direita temp)
		     nova-raiz))))))))

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


