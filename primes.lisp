
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))


(defun any (lst)
  (dolist (l lst)
    (if l (return-from any t))))

(defun any-satisfies (lst test)
  (dolist (l lst)
    (if (funcall test l) (return-from any t))))

(defun from-factors (factors)
  (apply #'* (mapcar
	      (lambda (p)
		(expt (car p) (cdr p)))
	      factors)))

(defun any-divides (num factors)
  (any-satisfies factors (lambda (f) (zerop (mod num f)))))

(defmacro divides (n d)
  `(zerop (mod n d)))

(defun next-prime (p)   ; of course only existing primes can be factors for check
			   			   
  (let* ((answer (+ p
     (if (= (mod p 6) 5)
	 2
	 4))))
    (if (is-prime answer)
	answer
	(next-prime answer))))

(defun prime-range (start limit)
  (let ((res))
    (do
     ((next (next-prime start) (next-prime next)))
     ((> next limit) (nreverse res))
      (push next res))))

(defun make-adjustable-vector (contents)
  (make-array (length contents) :fill-pointer t :adjustable t :initial-contents contents))

(defvar *primes* (make-adjustable-vector
		  '(2 3 5 7 11 13 17 19 23)))


(defun lastv (v)
  (aref v (1- (length v))))

(defun extend-primes-to (n)
  (let ((last (lastv *primes*)))
    (do ((next (next-prime last) (next-prime next)))
	((> next n))
      (vector-push-extend next *primes*))))

(defun binsert-left (vec val)
  (let ((high (1- (length vec)))
	(mid 0)
	(low 0))
    (while (<= low high)
      (setf mid (floor (/ (+ high low) 2)))
      (cond
	((< (aref vec mid) val) 
	 (setf low (+ mid 1)))
      
	((> (aref vec mid) val) 
	 (setf high (- mid 1)))

	(t (return-from binsert-left mid))))
    (if (< (aref vec mid) val)
	(+ mid 1)
	mid)))

(defun factor (n)
  (remove-if (lambda (p) (zerop (cdr p))) (full-factors n)))

(defun binsearch (vec val)
  (let ((pt (binsert-left vec val)))
    (and (< pt (length vec))
	 (= (aref vec pt) val))))

(defun extend-if-needed (limit)
  (if (< (lastv *primes*) limit)
      (extend-primes-to limit)))

(defun subseq-upto (vec val)
  (let* ((pt (binsert-left vec val))
	 (end (if (> (aref vec pt) val)
		  pt
		  (+ 1 pt))))
    (subseq vec 0 end)))
      
      

(defun primes-le (limit)
  (extend-if-needed limit)
  (coerce (subseq-upto *primes* limit) 'list))

(defun none-satisfy (pred lst)
  (dolist (l lst)
    (if (funcall pred l)
	(return-from not-any nil)))
  t)

(defun possibly-prime (n)
  (let ((m6 (mod n 6)))
    (cond
      ((< n 2) nil)
      ((= n 2) t)
      ((= n 3) t)
      ((= m6 1) t)
      ((= m6 5) t))))

(defun divides (n)
  (lambda (d) (zerop (mod n d))))

(defun is-known-prime (n)
  (and (< n (lastv *primes*)) (binsearch *primes* n)))

(defun is-prime (n)
  (if (possible-prime n)
      (or
       (is-known-prime n)
       (none-satisfy (divides n) (possible-factors n)))))

(defun possible-factors (n)
  (let ((limit (floor (sqrt n))))
    (primes-le limit)))

(defun full-factors (n)
  (let ((factors)
	(working n))
    (dolist (p (possible-factors n))
      (push `(,p . 0) factors)
      (while (zerop (mod working p))
	(setf working (/ working p))
	(incf (cdr (assoc p factors)))))
    (if (> working 1)
	(push `(,working . 1) factors))
    (nreverse factors)))

    
(defun coerce-to-string (something)
  (with-output-to-string (xx)
			 (format xx "~a" something)))
      
(defun palindrome-p (seq)
  (let* ((working (coerce-to-string seq))
	 (len (length working))
	 (lim (floor (/ len 2)))
	 (beginning (subseq working 0 lim))
	 (end (subseq working  (- len lim))))
    (string= beginning (reverse end))))

(defun prime-residues (n)
  (mapcar (lambda (p) (mod n p)) (possible-factors n)))
    



	 
	 
