
(in-package :com.gashu.kenji.destination-cost)


(let* ((cars-cost (mapcar #'parse-integer (my-split (read-line) :delimiterp (lambda (c) (char= c #\Space)))))))


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
