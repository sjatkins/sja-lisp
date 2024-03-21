(defun mults-while (base limit &key (while #'<=))
  (let ((res)
	(test (complement while)))
    (do (
	 (b base (+ b base)))
	((funcall test b limit) (nreverse res))
      (push b res))))

(defun mult-iter (base &optional (start-mult 0))
  (let ((current (* base start-mult)))
    (lambda () (incf current base))))

(defun mults-iter-to (base limit)
  (let ((mults (mult-iter base))
	(res))
    (do ((b (funcall mults) (funcall mults)))
	((> b limit) (nreverse res))
      (push b res))))

(defun check-answer (function answer &rest args)
  (let ((actual (apply function args)))
    (if (not (= answer actual))
	"broken - fix me")))

(defun check-expected (function &rest answers_args)
  (dolist (answer_args answers_args)
    (let ((answer) (car answer_args)))))
  
(defun combined-mults-sum (limit bases &key (while #'<))
  (let* ((mult-lists (mapcar
		      (lambda (b) (mults-while b limit :while while)) bases))
	 (nums (reduce
		(lambda (a b)
		  (union a b))
		mult-lists)))
    (apply #'+ nums)))

(defun fib-iter ()
  (let ((a -1) (b 1) (temp a))
    (lambda ()
      (setf temp a)
      (setf a b)
      (setf b (+ temp b)))))

(defun collect-iter-limit (iter limit while-condition)
  (let ((res))
    (do
     ((val (funcall iter) (funcall iter)))
     ((not (funcall while-condition val limit)) (nreverse res))
      (push val res))))
