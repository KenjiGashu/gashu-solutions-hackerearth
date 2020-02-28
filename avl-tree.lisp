(in-package :cl)

(defpackage :avl-tree
  (:use :cl))

(in-package :avl-tree)

(defclass avl-tree ()
  ((key
    :initarg :key
    :accessor key)
   (val
    :initarg :val
    :accessor val)
   (left
    :initarg :left
    :accessor left
    :initform nil)
   (right
    :initarg :right
    :accessor right
    :initform nil)
   (weight
    :initarg :weight
    :accessor weight))
  (:documentation "avl-tree"))

(defmethod print-object ((obj avl-tree) out)
  (print-unreadable-object (obj out :type t)
    (format out "K:~a" (key obj))))

(defun avl-height (tree h)
  (if (null tree)
      h
      (max (avl-height (left tree) (1+ h)) (avl-height (right tree) (1+ h)))))

(defclass queue ()
    ((start
      :initarg :start
      :accessor start
      :initform nil)
     (end
      :initarg :end
      :accessor end
      :initform nil))
  (:documentation "queue"))

(defmethod print-object ((obj queue) out)
  (print-unreadable-object (obj out :type t)
    (format out "(")
    (loop for x = (start obj) then (next x)
	  while (not (null x)) do
	  (format out "~a " (data x)))
    (format out ")")))

(defparameter temp1 (make-instance 'element :next nil :previous nil :data 0))
(defparameter temp2 (make-instance 'element :next nil :previous temp1 :data 1))
(setf (next temp1) temp2)
(defparameter temp3 (make-instance 'element :next nil :previous temp2 :data 2))
(setf (next temp2) temp3)
(defparameter q (make-instance 'queue))
(setf (start q) temp1)
(setf (end q ) temp3)
(setf q (insert-queue q 4))
(data (start q))
(data (end q))
(pop-queue q)
(format t "~a~%" q)

(loop for x = temp1 then (next x)
      while (not (null x)) do
      (format t "asidasd~%"))

(defclass element ()
    ((data
      :initarg :data
      :accessor data)
     (next
      :initarg :next
      :accessor next
      :initform nil)
     (previous
      :initarg :previous
      :accessor previous
      :initform nil)))

(defgeneric insert-queue (q elt))
(defmethod insert-queue ((q queue) elt)
  (with-accessors ((start start) (end end)) q
    (if (null start)
	(let* ((new-elt (make-instance 'element :data elt :next nil :previous nil)))
	  (make-instance 'queue :start new-elt :end new-elt))
	(let* ((new-elt (make-instance 'element :data elt :next nil :previous end)))
	  (setf (next (end q)) new-elt)
	  (setf (end q) new-elt)
	  q))))

(defgeneric pop-queue (q))
(defmethod pop-queue ((q queue))
  (if (null (start q))
      nil
      (let ((resp (start q)))
	(setf (start q) (next (start q)))
	(when (null (start q))
	  (setf (end q) nil))
	resp)))

(defparameter temp (make-instance 'queue :start nil :end nil))
(setf temp (insert-queue temp 9))
(insert-queue temp 9)
(start temp)
(pop-queue temp)
;; (defparameter temp (make-instance 'queue))
;; (setf (slot-value temp 'end) nil)
;; (format t "~a~%" temp)
;; (slot-exists-p temp 'end)

(defun print-tree (tree)
  (labels ((nos-na-altura (altura)
	     (expt 2 altura))
	   (liga-filhos (idx altura largura str)
	     (let* ((filhos-abaixo (nos-na-altura (1+ altura)))
		    (tam-abaixo (/ largura (1+ filhos-abaixo)))
		    (pos1 (* tam-abaixo (1- (* 2 idx))))
		    (pos2 (* tam-abaixo (* 2 idx))))
	       (replace "/" str :start1 pos1)
	       (replace "\\" str :start1 pos2)))))
  (let* ((height (avl-height tree 0))
	 (last-height height)
	 (line "")
	 (line2 "")
	 (pos 1)
	 (size 0))
    (if (null tree)
	nil
	(let ((fila (make-instance 'queue)))
	  (setf fila (insert-queue fila (cons 0 tree)))
	  (setf size (/ (* 2 (expt 2 height)) 2))
	  (setf line (format nil (format nil "~~~ad" (* 2 (expt 2 height))) 0))
	  (setf line2 (format nil (format nil "~~~ad" (* 2 (expt 2 height))) 0))
	  (format t "fila: ~a~%" fila)
	  (loop for n = (pop-queue fila) then (pop-queue fila)
		while (not (null n)) do
		  (progn
		    (let* ((alt (car (data n)))
			   (no (cdr (data n)))
			   (key-str (write-to-string (unless (null no)
						       (key no)))))
		      (unless (= last-height alt)
			(format t line)
			(format t "~%")
			(format t line2)
			(format t "~%")
			(setf line (format nil (format nil "~~~ad" (* 2 (expt 2 height))) 0))
			(setf line2 (format nil (format nil "~~~ad" (* 2 (expt 2 height))) 0))
			(setf pos 1)
			(setf size (floor (/ (* 2 (expt 2 height)) (+ (expt 2 alt) 1))))
			(setf last-height alt))
		      (unless (null no)
			(setf fila (insert-queue fila (cons (1+ alt) (left no)))))
		      (unless (null no)
			(setf fila (insert-queue fila (cons (1+ alt) (right no)))))
		      (unless (null no)
			(replace line key-str :start1 (* pos size)))
		      (unless (null (left no))
			(replace line2 "/" :start1 (- (* pos size) 1)))
		      (unless (null (right no))
			(replace line2 "\\" :start1 (+ (* pos size) 1))))
		    (setf pos (1+ pos))))))))

(defun nos-na-altura (altura)
  (expt 2 altura))

(defun liga-filhos (idx altura largura str)
  (let* ((filhos-abaixo (nos-na-altura (1+ altura)))
	 (tam-abaixo (/ largura (1+ filhos-abaixo)))
	 (tam-agora (/ largura (1+ (nos-na-altura altura))))
	 (pos1 (floor (/ (+ (* tam-abaixo (1- (* 2 idx))) (* tam-agora idx)) 2)))
	 (pos2 (floor (/ (+ (* tam-abaixo (* 2 idx)) (* tam-agora idx)) 2))))
    (format t "filhos-abaixo: ~a~%tam-abaixo: ~a~%tam-agora:~a~%pos1:~a~%pos2:~a~%" filhos-abaixo tam-abaixo tam-agora pos1 pos2)
    (replace str "/" :start1 pos1 :end1 (1+ pos1))
    (replace str "\\" :start1 pos2 :end1 (1+ pos2))
    (replace str "10" :start1 (floor (* idx tam-agora)))))

(liga-filhos 1 1 40 "                                        ")

(nth 3 "1234567")
(format t (format nil "~~~ad~~%" (expt 2 3)) 10)
(format t "~8d~%" 10)

(defun create-avl-tree (key &optional (val 0) (left nil) (right nil) (weight 0))
  (make-instance 'avl-tree :key key :val val :left left :right right :weight weight))

(defun rotate-left (node)
  (with-accessors ((left left) (right right) (key key) (val val)) (right node)
    (make-instance 'node :key key :val val
			:left (make instance 'node :left (left node)
						   :right left
						   :key (key node)
						   :val (val node)
						   :weight (max (weight (weight (left node)) (weight (eight node)))))
			:right right)))

(defun rotate-right (node)
  (with-accessors ((left left) (right right) (key key) (val val)) (left node)
    (make-instance 'node :key key :val val
			:left left
			:right (make instance 'node :left right
						   :right (right node)
						   :key (key node)
						   :val (val node)
						    :weight (max (weight (weight (left node)) (weight (eight node))))))))

(defun insert (node new)
  (if (null node)
      new
      (if (> (key new) (key node))
	  (multiple-value-bind (new-node rebalancep)
	      (insert (right node) new)
	    (if rebalancep
		(cond ((> (weight node) 0) (values (rebalance-node (create-avl-tree (key node) (val node) (left node) new-node (weight node))) t))
		      ((< (weight node) 0) (values (create-avl-tree (key node) (val node) (left node) new-node 0) nil))
		      (t (values (create-avl-tree (key node) (val node) (left node) new-node 1) t)))
		(values node nil)))
	  (multiple-value-bind (new-node rebalancep)
	      (insert (left node) new)
	    (if rebalancep
		(cond ((> (weight node) 0) (values (create-avl-tree (key node) (val node) new-node (right node) 0) nil))
		      ((< (weight node) 0) (values (rebalance-node (create-avl-tree (key node) (val node) new-node (right node) (weight node))) t))
		      (t (values (create-avl-tree (key node) (val node) (left node) new-node -1) t)))
		(values node nil))))))



;;mock tree for left right rotation (maybe)

(defparameter *no-35* (make-instance 'avl-tree :key 35 :val 35 :weight 0 :left nil :right nil))
(defparameter *no-20* (make-instance 'avl-tree :key 20 :val 20 :weight 1 :left nil :right *no-35*))
(defparameter *no-500* (make-instance 'avl-tree :key 500 :val 500 :weight 0 :left nil :right nil))
(defparameter *no-600* (make-instance 'avl-tree :key 600 :val 600 :weight 0 :left nil :right nil))
(defparameter *no-750* (make-instance 'avl-tree :key 750 :val 750 :weight -1 :left *no-600* :right nil))
(defparameter *no-5000* (make-instance 'avl-tree :key 5000 :val 5000 :weight 0))
(defparameter *no-1000* (make-instance 'avl-tree :key 1000 :val 1000 :weight -1 :left *no-750* :right *no-5000*))
;;(defparameter *no-700* (make-instance 'avl-tree key 700 :val 700 :weight 0))
(defparameter *no-550* (make-instance 'avl-tree :key 550 :val 550 :weight -1 :left *no-500* :right *no-1000*))
(defparameter *no-100* (make-instance 'avl-tree :key 100 :val 100 :weight 2 :left *no-25* :right *no-550*))


(defparameter *teste-insert* nil)
(insert *teste-insert* (create-avl-tree 1 1 nil nil 0 ))


;;teste avl-height
(defparameter *no-150* (make-instance 'avl-tree :key 150 :val 35 :weight 0 :left nil :right nil))
(defparameter *no-75* (make-instance 'avl-tree :key 75 :val 75 :weight 1 :left nil :right *no-150*))
(defparameter *no-3* (make-instance 'avl-tree :key 3 :val 3 :weight 0 :left nil :right nil))
(defparameter *no-4* (make-instance 'avl-tree :key 4 :val 4 :weight 0 :left *no-3* :right nil))
(defparameter *no-5* (make-instance 'avl-tree :key 5 :val 5 :weight 0 :left *no-4* :right nil))
(defparameter *no-15* (make-instance 'avl-tree :key 15 :val 15 :weight 1 :left *no-5* :right *no-75*))
(print-tree *no-15*)
(print-tree *no-150*)
(avl-height *no-15* 0)
(avl-height *no-75* 0)
(avl-height *no-150* 0)
;;not working btw
;; (defclass avl-tree ()
;;   ((key
;;     :initarg :node-key
;;     :reader node-key)
;;    (value
;;     :initarg :node-value
;;     :reader node-value)
;;    (left
;;     :initarg :left-child
;;     :reader left-child)
;;    (right
;;     :initarg :right-child
;;     :reader right-child)
;;    (weight
;;     :initarg :weight
;;     :reader weight))
;;   (:documentation
;;    "A node in an AVL tree."))

;; (defun make-node (key value left right)
;;   "Binary tree node with association and branches."
;;   (make-instance
;;    'avl-tree :node-key key :node-value (if (listp value)
;; 					   value
;; 					   (list value))
;;    :left-child left :right-child right :weight 0))

;; (defgeneric tree-height (tree))

;; (defmethod tree-height ((tree null))
;;   "Get the height of an empty TREE."
;;   0)

;; (defmethod tree-height ((tree avl-tree))
;;   "Get height of TREE."
;;   (1+ (max (tree-height (left-child tree))
;;            (tree-height (right-child tree)))))


;; (defun balance-factor (node)
;;   "Get balance factor of subtree rooted at NODE."
;;   (ecase (- (tree-height (right-child node))
;;             (tree-height (left-child node)))
;;     (-2 :imbalanced-left)
;;     (-1 :left-heavy)
;;     ( 0 :balanced)
;;     (+1 :right-heavy)
;;     (+2 :imbalanced-right)))


;; (defgeneric rotate-left (node))

;; (defmethod rotate-left ((node avl-tree))
;;   "Return TREE rotated left."
;;   (with-slots (key value height left right) node
;;     (make-node
;;      (node-key right)
;;      (node-value right)
;;      (avl-node key value
;;                left (left-child right))
;;      (right-child right))))

;; (defgeneric rotate-right (node))

;; (defmethod rotate-right ((node avl-tree))
;;   "Return TREE rotated right."
;;   (with-slots (key value height left right) node
;;     (make-node
;;      (node-key left)
;;      (node-value left)
;;      (left-child left)
;;      (avl-node key value
;;                (right-child left) right))))

;; (defun avl-node (key value &optional left right)
;;   "Balanced AVL tree node."
;;   (let ((node (make-node key value left right)))
;;     (ecase (balance-factor node)
;;       ((:left-heavy :balanced :right-heavy)
;;        node)

;;       (:imbalanced-left
;;        (ecase (balance-factor left)
;;          (:left-heavy
;;           (rotate-right node))
;;          (:right-heavy
;;           (rotate-right (make-node key value
;; 				   (rotate-left left) right)))))

;;       (:imbalanced-right
;;        (ecase (balance-factor right)
;;          (:left-heavy
;;           (rotate-left (make-node key value
;; 				  left (rotate-right right))))
;;          (:right-heavy
;;           (rotate-left node)))))))

;; (defgeneric lessp (a &rest rest))

;; (defmethod lessp ((a number) &rest rest)
;;   (apply #'< a rest))

;; (defgeneric insert (key value tree))

;; (defmethod insert (key value (tree null))
;;   "Insert pair of KEY and VALUE in an empty TREE."
;;   (avl-node key value nil nil))

;; (defmethod insert (key value (tree avl-tree))
;;   "Add an association from KEY to VALUE in TREE."
;;   (avl-node
;;    (node-key tree)
;;    (if (equalp key (node-key tree))
;;        (cons value (node-value tree))
;;        (node-value tree))
;;    (if (lessp key (node-key tree))
;;        (insert key value
;;                (left-child tree))
;;        (left-child tree))
;;    (if (lessp (node-key tree) key)
;;        (insert key value
;;                (right-child tree))
;;        (right-child tree))))

;; (defgeneric lookup (key tree))

;; (defmethod lookup (key (tree null))
;;   "Lookup KEY in the empty TREE."
;;   nil)

;; (defmethod lookup (key (tree avl-tree))
;;   "Return all values associated with KEY in TREE."
;;   (with-slots ((node-key key) value left right)
;;       tree
;;     (cond
;;       ((lessp key node-key) (lookup key left))
;;       ((lessp node-key key) (lookup key right))
;;       (t (cons value
;;                (append (lookup key left)
;;                        (lookup key right)))))))

;; (defgeneric pop-least (tree))

;; (defmethod pop-least ((tree null))
;;   nil)

;; (defmethod pop-least ((tree avl-tree))
;;   (with-slots ((node-key key) value left right) tree
;;     (if (null left)
;; 	(if (> (length value) 1)
;; 	    (values (avl-node
;; 		     node-key
;; 		     (cdr value)
;; 		     left
;; 		     right)
;; 		    (car value))
;; 	    (values right
;; 		    (car value)))
;; 	(multiple-value-bind (child ret) (pop-least left)
;; 	  (values (avl-node node-key
;; 			    value
;; 			    child
;; 			    right)
;; 		  ret)))))

;; (defmethod print-object ((obj avl-tree) out)
;;   (print-unreadable-object (obj out :type t)
;;     (format out "K:~a V:~a L:~:[NIL~;Branch~] R:~:[NIL~;Branch~]" (node-key obj) (node-value obj) (left-child obj) (right-child obj))))

;; (defparameter *word-map* nil)
;; (setq *word-map* nil)

;; (mapc (lambda (word)
;;         (setq *word-map*
;;               (insert (length word)
;;                       word
;;                       *word-map*)))
;;       '("hey" "goodbye" "hello"
;;         "hi" "world" "greetings"
;; 	"amskd" "mmsnn"))

;; (setf *word-map* (insert 3 "hey" *word-map*))
;; (setf *word-map* (insert 1 "h" *word-map*))
;; (setf *word-map* (insert 5 "heyus" *word-map*))
;; (setf *word-map* (insert 7 "heyksjd" *word-map*))

;; (tree-height *word-map*)

;; (node-value *word-map*)

;; (node-key *word-map*)
;; (left-child (left-child (left-child *word-map*)))

;; (pop-least *word-map*)
;; (multiple-value-bind (tree ret) (pop-least *word-map*)
;;   (setf *word-map* tree)
;;   (format t "~a ~a ~%" ret tree))

;; (lookup 5 *word-map*)

;; (with-open-file (in "destination-cost2.data" :direction :input)
;;   (let* ((cities (read in))
;; 	 (difference-cost-tree nil))
;;       ;;initialize bus-cost and difference cost tree
;;       (loop for i from 0 upto (1- cities) do
;; 	(let* ((val (read in)))
;; 	  (setf difference-cost-tree (insert val val difference-cost-tree)))
	
;; 	    )
;;     (format t "~a~%" difference-cost-tree)
;;       ))
