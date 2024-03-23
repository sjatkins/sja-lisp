

(defun from-factors (factors)
  (apply #'* (mapcar
	      (lambda (p)
		(expt (car p) (cdr p)))
	      factors)))

(defun any-divides (num factors)
  (any-satisfies factors (lambda (f) (zerop (mod num f)))))

(defmacro divides (n d)
  `(zerop (mod n d)))


(defun next-prime (p)
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

(defun last-known-prime ()
  (lastv *primes*))

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

(defun primes-le (limit)
  (ensure-primes-upto limit)
  (let ((pt (binsert-left *primes* limit)))
    (coerce
     (subseq
      *primes*
      0
      (if (> (aref *primes* pt) limit)
	  pt
	  (+ 1 pt)))
     'list)))
  
(defun is-prime (n)
  (if (< n (lastv *primes*))
      (return-from is-prime (binsearch *primes* n)))

  (dolist (p (possible-factors n))
      (if (zerop (mod n p))
	  (return-from is-prime nil)))
  t)

(defun fib ()
  (let ((a 0) (b 0))
    (lambda ()
      (cond
	((zerop a) (setf a 1))
	((and
	  (= a 1) (zerop b)) (setf b 2))
	(t (let ((temp (+ a b)))
	     (setf a b)
	     (setf b temp)))))))

(defun possible-factors (n)
  (let ((limit (floor (sqrt n))))
    (ensure-primes-upto limit)
    (primes-le limit)))

(defun largest-factor (n)
  (let ((rev-factors (nreverse (possible-factors n))))
    (dolist (p rev-factors)
      (if (zerop (mod n p))
	  (return-from largest-factor p)))
    n))


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
    



	 
	 
