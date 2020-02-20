(in-package :cl)

(defpackage :avl-tree
  (:use :cl))

(in-package :avl-tree)

(defclass nvl-treeame ()
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

(defun create-avl-tree (key &optional (val 0) (left nil) (right nil) (weight 0))
  (make-instance 'avl-tree :key key :val val :left left :right right :weight weight))


(defun rotate-left (node)
  (with-accessors ((left left) (right right) (key key) (val val)) (right node)
    (make-insance 'node :key key :val val
			:left (make instance 'node :left (left node)
						   :right left
						   :key (key node)
						   :val (val node)
						   :weight (max (weight (weight (left node)) (weight (eight node)))))
			:right right)))


;;mock tree

(defparameter *no-35* (make-instance 'node :key 35 :val 35 :weight 0 :leaft nil :right nil))
(defparameter *no-20* (make-instance 'node :key 20 :val 20 :weight 1 :left nil :right *no-35*))
(defparameter *no-500* (make-instance 'node :key 500 :val 500 :weight 0 :leaft nil :right nil))
(defparameter *no-600* (make-instance 'node :key 600 :val 600 :weight 0 :left nil :right nil))
(defparameter *no-750* (make-instance 'node :key 750 :val 750 :weight -1 :left *no-600* :right nil))
(defparameter *no-5000* (make-instance 'node :key 5000 :val 5000 :weight 0))
(defparameter *no-1000* (make-instance 'node :key 1000 :val 1000 :weight -1 :left *no-750* :right *no-5000*))
;;(defparameter *no-700* (make-instance 'node key 700 :val 700 :weight 0))
(defparameter *no-550* (make-instance 'node :key 550 :val 550 :weight -1 :left *no-500* :right *no-1000*))
(defparameter *no-100* (make-instance 'node :key 100 :val 100 :weight 2 :left *no-25* :eight *no-550*))

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
