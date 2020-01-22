(in-package :cl)

(defpackage :avl-tree
  (:use :cl))

(in-package :avl-tree)

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
   'avl-tree :node-key key :node-value (if (listp value)
					   value
					   (list value))
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
  (let ((node (make-node key value left right)))
    (ecase (balance-factor node)
      ((:left-heavy :balanced :right-heavy)
       node)

      (:imbalanced-left
       (ecase (balance-factor left)
         (:left-heavy
          (rotate-right node))
         (:right-heavy
          (rotate-right (make-node key value
				   (rotate-left left) right)))))

      (:imbalanced-right
       (ecase (balance-factor right)
         (:left-heavy
          (rotate-left (make-node key value
				  left (rotate-right right))))
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
   (if (equalp key (node-key tree))
       (cons value (node-value tree))
       (node-value tree))
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
	(if (> (length value) 1)
	    (values (avl-node
		     node-key
		     (cdr value)
		     left
		     right)
		    (car value))
	    (values right
		    (car value)))
	(multiple-value-bind (child ret) (pop-least left)
	  (values (avl-node node-key
			    value
			    child
			    right)
		  ret)))))

(defmethod print-object ((obj avl-tree) out)
  (print-unreadable-object (obj out :type t)
    (format out "K:~a V:~a L:~:[NIL~;Branch~] R:~:[NIL~;Branch~]" (node-key obj) (node-value obj) (left-child obj) (right-child obj))))

(defparameter *word-map* nil)
(setq *word-map* nil)

(mapc (lambda (word)
        (setq *word-map*
              (insert (length word)
                      word
                      *word-map*)))
      '("hey" "goodbye" "hello"
        "hi" "world" "greetings"
	"amskd" "mmsnn"))

(setf *word-map* (insert 3 "hey" *word-map*))
(setf *word-map* (insert 1 "h" *word-map*))
(setf *word-map* (insert 5 "heyus" *word-map*))
(setf *word-map* (insert 7 "heyksjd" *word-map*))

(tree-height *word-map*)

(node-value *word-map*)

(node-key *word-map*)
(left-child (left-child (left-child *word-map*)))

(pop-least *word-map*)
(multiple-value-bind (tree ret) (pop-least *word-map*)
  (setf *word-map* tree)
  (format t "~a ~a ~%" ret tree))

(lookup 5 *word-map*)

(with-open-file (in "destination-cost2.data" :direction :input)
  (let* ((cities (read in))
	 (difference-cost-tree nil))
      ;;initialize bus-cost and difference cost tree
      (loop for i from 0 upto (1- cities) do
	(let* ((val (read in)))
	  (setf difference-cost-tree (insert val val difference-cost-tree)))
	
	    )
    (format t "~a~%" difference-cost-tree)
      ))
