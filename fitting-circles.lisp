(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun my-delimiterp (c) (or (char= c #\Space) (char= c #\,)))

(defun delimiterp (c) (position c " ,.;/"))


(defparameter test-num (parse-integer (read-line)))
(dotimes (i test-num)
  (let* ((values (my-split (read-line) :delimiterp (lambda (c) (char= c #\Space))))
	 (a (parse-integer (car values)))
	 (b (parse-integer (cadr values))))
    (solve a b)))
;;(defparameter a 10)
;;(defparameter b 40)

(defun solve (a b)
  (let ((diameter (min a b)))
    (format t "~a~%" (floor
		      (max (/ a diameter)
			   (/ b diameter))))))

(with-open-file (in "fitting-circle.data" :direction :input)
  (setq test-num (parse-integer (read-line in)))
  ;;(format t "linha: ~a~%" (read-line in))
  (dotimes (i test-num)
    (let* ((values (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space))))
	   (a (parse-integer (car values)))
	   (b (parse-integer (cadr values))))
	   ;;)
      (solve a b))))
;;(solve a b)

;;(my-split "asd qwe qw eqweqweq weqw e" :delimiterp  (lambda (c) (char= c #\Space)))



;; ==================================================
;; Solution submitted
;; ==================================================

;; Sample code to perform I/O:
 
;; (setq name (read-line))                 ; Reading input from STDIN
;; (format t "Hi, ~F.~%" name)             ; Writing output to STDOUT
 
;; Warning: Printing unwanted or ill-formatted data to output will cause the test cases to fail
 
;; Write your code here
(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
 
(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))
 
(defun delimiterp (c) (position c " ,.;/"))
 
 
 
(defparameter test-num (parse-integer (read-line)))
 
;;(format t "oi")
 
;;(format t "~a~%" (my-split (read-line) :delimiterp (lambda (c) (char= c #\Space))))
 
;;(format t "~a~%" (parse-integer (car (my-split (read-line) :delimiterp (lambda (c) (char= c #\Space))))))
;;(format t "~a~%" (parse-integer (cadr (my-split (read-line) :delimiterp (lambda (c) (char= c #\Space))))))
 
(defun solve (a b)
  (let ((diameter (min a b)))
    (format t "~a~%" (floor
		      (max (/ a diameter)
			   (/ b diameter))))))
 
(dotimes (i test-num)
  (let* ((values (my-split (read-line) :delimiterp (lambda (c) (char= c #\Space))))
	 (a (parse-integer (car values)))
	 (b (parse-integer (cadr values))))
    (solve a b)))
;;(defparameter a 10)
;;(defparameter b 40)
