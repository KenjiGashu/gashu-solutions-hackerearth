(with-open-file (output "destination-cost2.data" :direction :output :if-exists :supersede)
  (let ((state (make-random-state)))
    (write-line "51783" output)
    (loop for i from 1 upto 51783 do
      (format output "~a " (random 1000000000))
      finally (format output "~a~%" (random 1000000000))
	  )
    (loop for i from 1 upto 51783 do
      (format output "~a " (random 1000000000))
      finally (format output "~a~%" (random 1000000000))
      )))


(dotimes (i 10)
  (let ((state (make-random-state)))
    (format t "~a~%" (random 1000000000))))


(let ((state1 (make-random-state))
       (state2 (make-random-state)))
   (= (random 1000 state1) (random 1000 state2)))
