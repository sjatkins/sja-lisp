(in-package #:sja)

(defun bytes-to-int (bytes)
  (let* ((num (length bytes))
         (ba (coerce bytes 'vector))
         (mults 
           (mapcar
            (lambda (i)
              (ash 1 (* 8 i)))
            (alexandria:iota num))))
    (apply #'+
           (mapcar
            (lambda (a b)
              (* a b))
            mults bytes))))
            
    
(defun encode-int-little (sz val)
  (let* ((vals
           (coerce
            (mapcar
             (lambda (i) (mod (ash val (* -8 i)) (ash 1 (* 8 (1+ i)))))
             (alexandria:iota 4)) 'vector ))
         (num 0)
         (bits (* 8 (- sz 1))))
    (dotimes (i sz)
      (setf
       (ldb (byte 8 (- bits (* 8 i))) num)
       (aref vals i)))
    num))

(defun decode-int-little (sz val)
  (let* ((nums (alexandria:iota sz))
         (mults (mapcar (lambda (i) (expt 256 i)) nums))
         (bits (* 8 (- sz 1)))
         (vals (mapcar (lambda (b) (ldb (byte 8 (- bits (* b 8))) val)) nums)))
    
    (apply #'+ (mapcar (lambda (a b) (* a b)) vals mults))))
