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

(with-open-file (in "destination-cost.data" :direction :input)
  (let* ((cities (parse-integer (read-line in)))
	 (cars-cost (mapcar #'parse-integer (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
	 (bus-cost (mapcar #'parse-integer (my-split (read-line in) :delimiterp (lambda (c) (char= c #\Space)))))
	 (min nil))
    (format t "cities: ~a cars-cost: ~a bus-cost: ~a~%" cities cars-cost bus-cost)
    (format t "~a~%" (solve cities cars-cost bus-cost 0 '() 0))))


