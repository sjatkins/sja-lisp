(in-package #:sja)

(defun iter-filter (iter test)
  (let ((item (funcall item)))
    (defun inner ()
      (if (and item (funcall test item))
	  (return-from inner item)
	  (setf item (funcall item))))))

(defun take-n (iter n)
  (let ((count n))
    (lambda ()
      (if count
	  (let ((item (funcall iter)))
	    (when (item)
	      (decf count)
	      item))))))
	   

(defun list-n (iter n)
  (let ((res))
    (dotimes (i n)
      (let ((item (funcall iter)))
	(if item
	    (push item res)
	    (return-from list-n (nreverse res)))))
    (reverse res)))

(defun iter-while (iter test)
  (lambda ()
    (let ((next (funcall iter)))
      (if (test next) next))))

(defun list-while (iter test)
  (let ((res)
	(next (funcall iter)))
    (while (and next (funcall test next))
      (push next res)
      (setf next (funcall iter)))
    (nreverse res)))

(defun list-le (iter limit)
  (list-while iter (lambda (x) (<= x limit))))

(defun sorted-le (lst limit)
  (let ((res))
    (dolist (l lst)
      (if (<= l limit) (push l res) (return-from sorted_le (nreverse res))))))

(defun chain (&rest iters)
  (lambda ()
    (dolist (iter iters)
      (let ((item (funcall iter)))
	(when (item)
	  
    
