(in-package :cl)
(defpackage :com.gashu.kenji.destination-cost
  (:use :cl))

(in-package :com.gashu.kenji.destination-cost)

(declaim (optimize (debug 3)))

(defclass avl-tree ()
  ((key
    :initarg :node-key
    :reader node-key)
   (value
    :initarg :node-value
    :reader node-value)
   (left
    :initarg :left-child
    :reader left-child)
   (right
    :initarg :right-child
    :reader right-child)
   (weight
    :initarg :weight
    :reader weight))
  (:documentation
   "A node in an AVL tree."))

(defun make-node (key value left right)
  "Binary tree node with association and branches."
  (make-instance
   'avl-tree :node-key key :node-value value
   :left-child left :right-child right :weight 0))

(defgeneric tree-height (tree))

(defmethod tree-height ((tree null))
  "Get the height of an empty TREE."
  0)

(defmethod tree-height ((tree avl-tree))
  "Get height of TREE."
  (1+ (max (tree-height (left-child tree))
           (tree-height (right-child tree)))))

(defun balance-factor (node)
  "Get balance factor of subtree rooted at NODE."
  (ecase (- (tree-height (right-child node))
            (tree-height (left-child node)))
    (-2 :imbalanced-left)
    (-1 :left-heavy)
    ( 0 :balanced)
    (+1 :right-heavy)
    (+2 :imbalanced-right)))


(defgeneric rotate-left (node))

(defmethod rotate-left ((node avl-tree))
  "Return TREE rotated left."
  (with-slots (key value height left right) node
    (make-node
     (node-key right)
     (node-value right)
     (avl-node key value
               left (left-child right))
     (right-child right))))

(defgeneric rotate-right (node))

(defmethod rotate-right ((node avl-tree))
  "Return TREE rotated right."
  (with-slots (key value height left right) node
    (make-node
     (node-key left)
     (node-value left)
     (left-child left)
     (avl-node key value
               (right-child left) right))))

(defun avl-node (key value &optional left right)
  "Balanced AVL tree node."
  (let* ((node (make-node key value left right)))
    (ecase (balance-factor node)
      ((:left-heavy :balanced :right-heavy)
       node)

      (:imbalanced-left
       (ecase (balance-factor left)
         (:left-heavy
          (rotate-right node))
	 (:balanced
	  (rotate-right node))
         (:right-heavy
          (rotate-right (make-node key value
				   (rotate-left left) right)))))

      (:imbalanced-right
       (ecase (balance-factor right)
         (:left-heavy
          (rotate-left (make-node key value
				  left (rotate-right right))))
	 (:balanced
	  (rotate-left node))
         (:right-heavy
          (rotate-left node)))))))

(defgeneric lessp (a &rest rest))

(defmethod lessp ((a number) &rest rest)
  (apply #'< a rest))

(defgeneric insert (key value tree))

(defmethod insert (key value (tree null))
  "Insert pair of KEY and VALUE in an empty TREE."
  (avl-node key value nil nil))

(defmethod insert (key value (tree avl-tree))
  "Add an association from KEY to VALUE in TREE."
  (avl-node
   (node-key tree)
   (node-value tree)
   (if (lessp key (node-key tree))
       (insert key value
               (left-child tree))
       (left-child tree))
   (if (lessp (node-key tree) key)
       (insert key value
               (right-child tree))
       (right-child tree))))

(defgeneric lookup (key tree))

(defmethod lookup (key (tree null))
  "Lookup KEY in the empty TREE."
  nil)

(defmethod lookup (key (tree avl-tree))
  "Return all values associated with KEY in TREE."
  (with-slots ((node-key key) value left right)
      tree
    (cond
      ((lessp key node-key) (lookup key left))
      ((lessp node-key key) (lookup key right))
      (t (cons value
               (append (lookup key left)
                       (lookup key right)))))))

(defgeneric pop-least (tree))

(defmethod pop-least ((tree null))
  nil)

(defmethod pop-least ((tree avl-tree))
  (with-slots ((node-key key) value left right) tree
    (if (null left)
	(if (listp value)
	    (if (> (length value) 1)
		(values (avl-node
			 node-key
			 (cdr value)
			 left
			 right)
			(car value))
		(multiple-value-bind (child ret) (pop-least right)
		  (values (avl-node node-key
				    value
				    child
				    right)
			  ret)))
	    (values right value))
	(multiple-value-bind (child ret) (pop-least left)
	  (values (avl-node node-key
			    value
			    child
			    right)
		  ret)))))

(defmethod print-object ((obj avl-tree) out)
  (print-unreadable-object (obj out :type t)
    (format out "K:~a V:~a L:~:[NIL~;Branch~] R:~:[NIL~;Branch~]" (node-key obj) (node-value obj) (left-child obj) (right-child obj))))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
	  :then (position-if-not delimiterp string :start (1+ end))
	:for end = (and beg (position-if delimiterp string :start beg))
	:when beg :collect (subseq string beg end)
	  :while end))


(defparameter test-filename "destination-cost.data")

;; (defparameter cities (parse-integer (read-line)))

(defun solve ()
  (let* ((cities (read))
	 (cars-cost (make-array (list cities) :element-type 'fixnum))
	 (bus-cost (make-array (list cities) :element-type 'fixnum))
	 (difference-cost-tree nil)
	 (total-car 0)
	 (total-bus 0)
	 (resp 0))
    (declare (type (array fixnum 1) cars-cost bus-cost)
	     (optimize (speed 3) (safety 0) (debug 0)))
    ;;initialize cars-cost
    (loop for i from 0 upto (1- cities) do
      (setf (aref cars-cost i) (the fixnum (read))))

    ;;initialize bus-cost and difference cost tree
    (loop for i from 0 upto (1- cities) do
      (let* ((car (aref cars-cost i))
	     (bus (setf (aref bus-cost i) (the fixnum (read))))
	     (dif (abs (- car bus))))
	(declare (type fixnum car bus dif))

	;;only add to cost tree when they have different costs
	(unless (= car
		   bus)
	  (setf difference-cost-tree (insert dif dif difference-cost-tree)))

	;;adds min between car and busm when they are the same, dont add to count

	(setf resp (+ resp (cond ((< car bus) (progn (setf total-car (1+ total-car))
						     car))
				 ((< bus car) (progn (setf total-bus (1+ total-bus))
						     bus))
				 (t car))))))

    (loop for i from 0 upto (- (abs (- total-car total-bus)) 2) do
      (multiple-value-bind (tree ret) (pop-least difference-cost-tree)
	(setf resp (+ resp ret))
	(setf difference-cost-tree tree)))
    (format t "~a~%" resp)
    )
  )

(defparameter *gtree* nil)

(defun merge-sort-merge (array1 array2)
  (let* ((len1 (array-dimension array1 0))
	 (len2 (array-dimension array2 0))
	 (merged-len (+ len1
			len2))
	 (merged (make-array (list merged-len)))
	 (i 0)
	 (j 0))
    (loop repeat merged-len
	  for index = 0 then (1+ index)
	  do
	     (cond ((and (< i len1) (< j len2))
		    (let* ((left (aref array1 i))
			   (right (aref array2 j)))
		      (if (< left right)
			  (progn
			    (setf (aref merged index) left)
			    (setf i (1+ i)))
			  (progn
			    (setf (aref merged index) right)
			    (setf j (1+ j)))
			  )))
		   ((< i len1)
		    (progn
		      (setf (aref merged index) (aref array1 i))
		      (setf i (1+ i))))
		   ((< j len2)
		    (progn
		      (setf (aref merged index) (aref array2 j))
		      (setf j (1+ j))))
		   (t (error "ue, ta iterando mais que o tamanho total do array")))
	  )
    merged))

(defun merge-sort (array)
  (labels ((internal-merge-sort (array start end)
	     (if (= start end)
		 (make-array '(1) :initial-element (aref array start))
		 (let* ((half (floor (/ (+ start end) 2)))
			(left (internal-merge-sort array start half))
			(right (internal-merge-sort array (1+ half) end)))
		   (merge-sort-merge left right)))))
    (internal-merge-sort array 0 (1- (fill-pointer array)))))



(defun solve-from-file ()
  (with-open-file (in "destination-cost.data" :direction :input)
    (let* ((cities (read in))
	   (cars-cost (make-array (list cities) :element-type 'fixnum))
	   (bus-cost (make-array (list cities) :element-type 'fixnum))
	   (dif-cost (make-array (list cities) :element-type 'fixnum))
	   (difference-cost-tree nil)
	   (total-car 0)
	   (total-bus 0)
	   (resp 0))
      ;;initialize cars-cost
      (loop for i from 0 upto (1- cities) do
	(setf (aref cars-cost i) (the fixnum (read in))))

      ;;initialize bus-cost and difference cost tree
      (loop for i from 0 upto (1- cities) do
	(let* ((car (aref cars-cost i))
	       (bus (setf (aref bus-cost i) (the fixnum (read in))))
	       (dif (abs (- car bus))))
	  (declare (type fixnum car bus dif))

	  ;;only add to cost tree when they have different costs
	  (unless (= car
		     bus)
	    (setf (aref dif-cost i) dif)
	    ;;(setf *gtree* (insert dif dif *gtree*))
	    )

	  ;;adds min between car and busm when they are the same, dont add to count

	  (setf resp (+ resp (cond ((< car bus) (progn (setf total-car (1+ total-car))
						       car))
				   ((< bus car) (progn (setf total-bus (1+ total-bus))
						       bus))
				   (t car))))))

      ;; (setf difference-cost-tree *gtree*)
      ;; (loop for i from 0 upto (- (abs (- total-car total-bus)) 2) do
      ;; 	(multiple-value-bind (tree ret) (pop-least *gtree*)
      ;; 	  (setf resp (+ resp ret))
      ;; 	  (setf *gtree* tree)))
      (format t "~a~%" resp)
      )))
(defun solve ()

    (let* ((cities (read))
	   (cars-cost (make-array (list cities) :element-type 'fixnum))
	   (bus-cost (make-array (list cities) :element-type 'fixnum))
	   (dif-to-car-cost (make-array (list cities) :element-type 'fixnum :adjustable t :fill-pointer 0))
	   (dif-to-bus-cost (make-array (list cities) :element-type 'fixnum :adjustable t :fill-pointer 0))
	   (difference-cost-tree nil)
	   (total-car 0)
	   (total-bus 0)
	   (resp 0))
      ;;initialize cars-cost
      (loop for i from 0 to (1- cities) do
	(setf (aref cars-cost i) (the fixnum (read))))

      ;;initialize bus-cost and difference cost tree
      (loop for i from 0 to (1- cities) do
	(let* ((car (aref cars-cost i))
	       (bus (setf (aref bus-cost i) (the fixnum (read))))
	       (dif (abs (- car bus))))
	  ;;(declare (type fixnum car bus dif))

	  ;;only add to cost tree when they have different costs
	  (unless (= car
		     bus)
	    (if (> car bus)
		(vector-push dif dif-to-car-cost)
		(vector-push dif dif-to-bus-cost))
	    
	    ;;(setf *gtree* (insert dif dif *gtree*))
	    )

	  ;;adds min between car and busm when they are the same, dont add to count

	  (setf resp (+ resp (cond ((< car bus) (progn (setf total-car (1+ total-car))
						       car))
				   ((< bus car) (progn (setf total-bus (1+ total-bus))
						       bus))
				   (t car))))))
      (setf dif-to-car-cost (merge-sort dif-to-car-cost)) 
      (setf dif-to-bus-cost (merge-sort dif-to-bus-cost))
      (loop repeat (- (floor (/ cities 2)) (min total-bus total-car))
	    for i = 0 then (1+ i)
	    do
	       (if (> total-bus total-car)
		   (setf resp (+ resp (aref dif-to-car-cost i)))
		   (setf resp (+ resp (aref dif-to-bus-cost i)))))
      ;; (setf difference-cost-tree *gtree*)
      ;; (loop for i from 0 upto (- (abs (- total-car total-bus)) 2) do
      ;; 	(multiple-value-bind (tree ret) (pop-least *gtree*)
      ;; 	  (setf resp (+ resp ret))
      ;; 	  (setf *gtree* tree)))
      (format t "~a~%" resp)
      ))
(defun solve-from-file2 ()
  (with-open-file (in "destination-cost4.data" :direction :input)
    (let* ((cities (read in))
	   (cars-cost (make-array (list cities) :element-type 'fixnum))
	   (bus-cost (make-array (list cities) :element-type 'fixnum))
	   (dif-to-car-cost (make-array (list cities) :element-type 'fixnum :adjustable t :fill-pointer 0))
	   (dif-to-bus-cost (make-array (list cities) :element-type 'fixnum :adjustable t :fill-pointer 0))
	   (difference-cost-tree nil)
	   (total-car 0)
	   (total-bus 0)
	   (resp 0))
      ;;initialize cars-cost
      (loop for i from 0 to (1- cities) do
	(setf (aref cars-cost i) (the fixnum (read in))))

      ;;initialize bus-cost and difference cost tree
      (loop for i from 0 to (1- cities) do
	(let* ((car (aref cars-cost i))
	       (bus (setf (aref bus-cost i) (the fixnum (read in))))
	       (dif (abs (- car bus))))
	  ;;(declare (type fixnum car bus dif))

	  ;;only add to cost tree when they have different costs
	  (unless (= car
		     bus)
	    (if (> car bus)
		(vector-push dif dif-to-car-cost)
		(vector-push dif dif-to-bus-cost))
	    
	    ;;(setf *gtree* (insert dif dif *gtree*))
	    )

	  ;;adds min between car and busm when they are the same, dont add to count

	  (setf resp (+ resp (cond ((< car bus) (progn (setf total-car (1+ total-car))
						       car))
				   ((< bus car) (progn (setf total-bus (1+ total-bus))
						       bus))
				   (t car))))))
      (setf dif-to-car-cost (merge-sort dif-to-car-cost)) 
      (setf dif-to-bus-cost (merge-sort dif-to-bus-cost))
      (format t "total-car: ~a total-bus: ~a~% to-car: ~a to-bus: ~a ~%" total-car total-bus dif-to-car-cost dif-to-bus-cost)

      (loop repeat (- (floor (/ cities 2)) (min total-bus total-car))
	    for i = 0 then (1+ i)
	    do
	       (if (> total-bus total-car)
		   (setf resp (+ resp (aref dif-to-car-cost i)))
		   (setf resp (+ resp (aref dif-to-bus-cost i)))))
      ;; (setf difference-cost-tree *gtree*)
      ;; (loop for i from 0 upto (- (abs (- total-car total-bus)) 2) do
      ;; 	(multiple-value-bind (tree ret) (pop-least *gtree*)
      ;; 	  (setf resp (+ resp ret))
      ;; 	  (setf *gtree* tree)))
      (format t "~a~%" resp)
      )))


(defun solve2 ()
  (let* ((cities (read))
	 (cars-cost (make-array (list cities) :element-type 'fixnum))
	 (bus-cost (make-array (list cities) :element-type 'fixnum))
	 (dif-cost (make-array (list cities) :element-type 'fixnum :adjustable t :fill-pointer 0))
	 (difference-cost-tree nil)
	 (total-car 0)
	 (total-bus 0)
	 (resp 0))
    ;;initialize cars-cost
    (loop for i from 0 upto (1- cities) do
      (setf (aref cars-cost i) (the fixnum (read))))

    ;;initialize bus-cost and difference cost tree
    (loop for i from 0 upto (1- cities) do
      (let* ((car (aref cars-cost i))
	     (bus (setf (aref bus-cost i) (the fixnum (read))))
	     (dif (abs (- car bus))))
	;;(declare (type fixnum car bus dif))

	;;only add to cost tree when they have different costs
	(unless (= car
		   bus)
	  (vector-push dif dif-cost)
	  ;;(setf *gtree* (insert dif dif *gtree*))
	  )

	;;adds min between car and busm when they are the same, dont add to count

	 (setf resp (+ resp (cond ((< car bus) (progn (setf total-car (1+ total-car))
	 					     car))
	 			 ((< bus car) (progn (setf total-bus (1+ total-bus))
	 					     bus))
	 			 (t car))))
	))

    

    (setf dif-cost (merge-sort dif-cost)) 
    (loop for i from 0 to (1- (floor (/ cities 2)))
	  do
	     (setf resp (+ resp (aref dif-cost i))))
    ;; (setf difference-cost-tree *gtree*)
    ;; (loop for i from 0 upto (- (abs (- total-car total-bus)) 2) do
    ;; 	(multiple-value-bind (tree ret) (pop-least *gtree*)
    ;; 	  (setf resp (+ resp ret))
    ;; 	  (setf *gtree* tree)))
    (format t "~a~%" resp)
    )
  )

(solve-from-file2)

;; (defun solve (cities cars-cost bus-cost step answer total-cost)
;;   (if (< step cities)
;;       (min (solve cities cars-cost bus-cost (1+ step) (cons (cons (nth step cars-cost) 'car) answer) (+ (nth step cars-cost) total-cost))
;; 	   (solve cities cars-cost bus-cost (1+ step) (cons (cons (nth step bus-cost) 'car) answer) (+ (nth step bus-cost) total-cost)))
;;       total-cost))

;; (defun solve2 (cities cars-cost bus-cost step answer total-cost)
;;   (if (< step cities)
;;       (solve2 cities
;; 	      cars-cost
;; 	      bus-cost
;; 	      (1+ step)
;; 	      (cons (min (aref cars-cost step)
;; 			 (aref bus-cost step))
;; 		    answer)
;; 	      (+ total-cost (min (aref cars-cost step)
;; 				 (aref bus-cost step))))
;;       total-cost))

;; (defclass arvore ()
;;   ((peso
;;     :accessor peso
;;     :initarg :peso
;;     :initform 0
;;     :documentation "peso das folhas")
;;    (esquerda
;;     :accessor esquerda
;;     :initform '()
;;     :initarg :esquerda)
;;    (direita
;;     :accessor direita
;;     :initform '()
;;     :initarg :direita)
;;    (valor
;;     :accessor valor
;;     :initform '()
;;     :initarg :valor)))

;; (defmethod print-object ((obj arvore) out)
;;   (print-unreadable-object (obj out :type t)
;;     (format out "V:~a W:~a L:~:[NIL~;Branch~] R:~:[NIL~;Branch~]" (valor obj) (peso obj) (esquerda obj) (direita obj))))
;; (defparameter *tree* '())
;; (defparameter *tree-instance* (make-instance 'arvore))
;; (setf (slot-value *tree-instance* 'valor) (cons 10 (slot-value *tree-instance* 'valor)))
;; (setf (slot-value *tree-instance* 'direita) (make-instance 'arvore :valor '(50) :esquerda (make-instance 'arvore :valor '(25))))
;; (defparameter *temp*
;;   (with-accessors ((direita direita) (valor valor) (peso peso)) *tree-instance*
;;     (let ((filho-direito direita))
;;       (with-accessors ((direita-direita direita) (direita-esquerda esquerda)) direita
;;       (let ((temp direita-esquerda))
;; 	(setf direita-esquerda *tree-instance*)
;; 	(setf direita temp)
;; 	(format t "temp: ~a~%" temp)))
;;     filho-direito)))

;; (defparameter *temp* (balance-tree *tree-instance*))
;; (slot-value (slot-value *temp* 'esquerda) 'direita)
;; (slot-value *temp* 'direita)

;; (slot-value (slot-value *tree-instance* 'esquerda) 'direita)
;; (slot-value (slot-value *tree-instance* 'direita) 'esquerda)
;; (slot-value *tree-instance* 'direita)


;; (insert-tree *temp* 80)

;; (with-accessors ((esquerda esquerda) (direita direita) (valor valor)) *temp*
;;   (with-accessors ((eval valor)) esquerda
;;     (with-accessors ((direita-esquerda esquerda) (valorzim valor)) direita
;; 	valor)
;;     ))

;; (with-accessors ((direita direita)) *tree-instance*
;;   (with-accessors ((direita-esquerda direita)) direita
;;     (setf direita-esquerda *tree-instance*)))


;; (with-accessors ((direita direita)) *tree-instance*
;;   (with-accessors ((direita-esquerda direita)) direita
;;     (slot-value direita-esquerda 'valor)))

;; (with-slots (direita esquerda valor) *tree-instance*
;;   (format t "~a~% ~a  ~%" valor (slot-value direita 'valor)))

;; (with-slots (direita esquerda valor) *tree-instance*
;;   direita)

;; (with-slots (direita esquerda valor) *temp*
;;   (format t "~a~% ~a  ~a ~%" valor (slot-value esquerda 'valor) (slot-value direita 'valor)))

;; (with-slots (direita esquerda valor) *temp*
;;   esquerda)



;; (defgeneric tree-contents (tree))
;; (defmethod tree-contents ((tree arvore))
;;   (slot-value tree 'valor))

;; (defgeneric tree-contents-value (tree))
;; (defmethod tree-contents-value ((tree arvore))
;;   (car (slot-value tree 'valor)))

;; (defgeneric balance-tree (tree))
;; (defmethod balance-tree ((tree arvore))
;;   (with-accessors ((esquerda esquerda) (direita direita) (peso peso) (valor valor)) tree
;;     (cond ((> peso 1)
;; 	   (with-accessors ((esquerda-direita direita) (esquerda-esquerda esquerda)) esquerda
;; 	     (with-accessors ((ede esquerda) (edd direita)) esquerda-direita
;; 	       (if (< (peso esquerda) 0)
;; 		   (let ((nova-raiz esquerda-direita))
;; 		     (break)
;; 		     (setf ede esquerda)
;; 		     (setf esquerda-direita edd)
;; 		     (setf esquerda edd)
;; 		     (setf edd tree)
;; 		     nova-raiz)
;; 		   (let ((nova-raiz esquerda)
;; 			 (temp esquerda-direita))
;; 		     (setf esquerda esquerda-direita)
;; 		     (setf esquerda-direita tree)
;; 		     nova-raiz)))))

;; 	  ((< peso -1)
;; 	   (with-accessors ((direita-esquerda esquerda)) direita
;; 	     (with-accessors ((ded direita) (dee esquerda)) direita-esquerda
;; 	       (if (> (slot-value direita 'peso) 0)
;; 		   (let ((nova-raiz direita-esquerda))
;; 		     (break)
;; 		     (setf direita-esquerda ded)
;; 		     (setf ded direita)
;; 		     (setf direita dee)
;; 		     (setf dee tree)
;; 		     nova-raiz)
;; 		   (let ((nova-raiz direita)
;; 			 (temp direita-esquerda))
;; 		     (setf direita-esquerda tree)
;; 		     (setf direita temp)
;; 		     nova-raiz))))))))
;; (defgeneric insert-tree (tree val))
;; (defmethod insert-tree ((tree arvore) val)
;;   (with-accessors ((valor valor) (peso peso) (esquerda esquerda) (direita direita)) tree
;;     (cond ((> val (car valor)) (progn
;; 				 (setf peso (1+ peso))
;; 				 (setf direita (insert-tree direita val))))
;; 	  ((< val (car valor)) (progn
;; 				 (setf peso (1- peso))
;; 				 (setf esquerda (insert-tree esquerda val))))
;; 	      (t (setf valor (cons val valor))))
;;     (balance-tree tree)))
;; (defmethod insert-tree ((tree null) val)
;;   (make-instance 'arvore :valor (list val)))

;; (defun leaf-val (tree)
;;   "I don't think I use this function"
;;   (car tree))

;; (defun value-tree (tree)
;;   "returns the value stored on leaf.
;; 1- If its a list, gets the first elt from the list
;; 2- If its a elt, just return it"
;;   (let ((val (car tree)))
;;     (if (listp val)
;; 	(car val)
;; 	val)))

;; (defun left-branch (tree)
;;   "returns left branch"
;;   (car (cdr tree)))

;; (defun right-branch (tree)
;;   "returns the right branch"
;;   (cdr (cdr tree)))

;; (defun make-leaf (elt)
;;   "make leaf with elt, right and left branch will be nil"
;;   (cons elt (cons '() '())))

;; (defun make-tree (elt left right)
;;   "makes tree using elt, left and right"
;;   (cons elt (cons left right)))

;; (defun insert-tree (tree elt)
;;   "insert element into the right branch of tree
;; when the elt already exists on tree, add it to the leaf who has the same elt (doesn't make another leaf)"
;;   (if (null tree)
;;       (make-leaf elt)
;;       (cond ((> elt (value-tree tree)) (make-tree (leaf-val tree) (left-branch tree) (insert-tree (right-branch tree) elt)))
;; 	    ((< elt (value-tree tree)) (make-tree (leaf-val tree) (insert-tree (left-branch tree) elt) (right-branch tree)))
;; 	    (t (make-tree (insert-linkedlist (car tree) elt) (left-branch tree) (right-branch tree))))))

;; (defun insert-linkedlist (l elt)
;;   "adds elt to start o linked list (l)
;;    if l is an element, make it a list"
;;   (if (listp l)
;;       (cons elt l)
;;       (list l elt)))

;; (defun clear-tree ()
;;   "clear the global parameter *teste-tree*"
;;   (setf *teste-tree* '()))

;; (defun find-tree (tree val)
;;   "returns value val when it exist inside tree and nil otherwise.
;; Always returns two values:
;; 1- elt
;; 2- leaf where it was found"
;;   (if (null tree)
;;       (values '() '())
;;       (cond ((> val (value-tree tree)) (find-tree (right-branch tree) val))
;; 	    ((< val (value-tree tree)) (find-tree (left-branch tree) val))
;; 	    (t (values (value-tree tree) tree)))))

;; (defun pop-value (tree val)
;;   (if (null tree)
;;       (values '() '())
;;       (cond ((> val (value-tree tree)) (make-tree (leaf-val tree) (left-branch tree) (pop-value (right-branch tree) val)))
;; 	    ((< val (value-tree tree)) (make-tree (leaf-val tree) (pop-value (left-branch tree) val) (right branch-tree)))
;; 	    (t (values (value-tree tree) tree)))))


