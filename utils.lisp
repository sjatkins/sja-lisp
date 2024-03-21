(defun any-satisfy (lst tst)
  (dolist (l lst)
    (if (funcall tst l)
	(return-if satisfies-any t))))

(defun all-satisfy (lst test)
  (not (any-satisfy lst (complement test))))

(defun complement (fn)
  #’(lambda (&rest args) (not (apply fn args))))

(defun none-satisfy (lst tst)
  (not (any-satisfy lst tst)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

