;;
;; except for palindrom-p these functions are for numeric
;; palindromes of length 2 * n
;;

(defun palindrome-p (seq)
  (let* ((working (coerce-to-string seq))
	 (len (length working))
	 (lim (floor (/ len 2)))
	 (beginning (subseq working 0 lim))
	 (end (subseq working  (- len lim))))
    (string= beginning (reverse end))))

(defun ends-with-digit (num b)
  (= d (mod n 10)))

(defun comp9 (n)
  (floor 9 (mod n 10)))

(defun lower-9-factor (n)
  (let* ((d (mod n 10))
	 (base (- n d)))
    (cond
      ((> d 3) (+ base 3))
      ((> d 1) (+ base 1))
      ((<= d 1) (- base 1)))))

(defun ok-num (x)
  (or (ends-with-digit 1)
      (ends-with-digit 3)
      (ends-with-digit 9)))

(defun find-m1 (start &optional (next-lower t))
  (let ((next-lower (or next-lower (not (ok-num start)))))
    (if next-ower
	(lower-9-factor start)
	start)))

(defun f2-for-f1 (f1)
  (let* ((comp (comp9 f1))
	 (f2 (+ (- max (mod max 10)) comp)))
    (while t
      (let ((prod (* f1 f2)))
	(if (> prod min)
	    (if (palindrome prod)
		(return-from f2-for-f1 f2))
	    (return-from f2-for-f1 nil))
	(decf f2 10)))))

(defun largest-numerical-palindrom-pair (n-digits)
  (let* ((max (1- (expt 10 n-digits)))
	 (abs-max (expt 10 (* 2 n-digits)))
	 (min (- abs-max (floor abs-max 10)))
	 (m (float (- max (mod max 11))))
	 (m1 (find-m1 m nil))
	 (f2))
      (while (>= m1 1)
	(setf f1 (* 11 m1))
	(setf f2 (f2-for-f1 f1))
	(if f2
	    (return-from largest-numerical-palindrome-pair (values f1 f2)))
	(setf m1 (find-m1 m1)))
      (values 0 0)))
	     
