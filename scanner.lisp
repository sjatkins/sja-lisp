(in-package #:sja)

(defclass line-reader ()
  ((lines :reader lines :initarg :lines)
   (line-pos :accessor line-pos :initform 0)
   (pos :accessor pos :initform 0)))

(defgeneric done (lr))

(defmethod done ((lr line-reader))
  (> (line-pos lr) (length (lines lr))))

(defmethod next-line (lr)
  (incf (line-pos lr))
  (setf (pos lr) 0)
  (current-line lr))

(defmethod current-line (lr)
  (if (done lr) '() (aref (lines lr) (line-pos lr))))

(defmethod line (lr)
  (current-line lr))


(defmethod current-char (lr)
  (let ((line (current-line lr)))
    (if line (char line (pos lr)))))

(defmethod next-char (lr)
  (let*
      (
       (line (current-line lr))
       (len (length line)))
    (incf (pos lr))
    (if (>= (pos lr) len)
      (next-line lr))
    (current-char lr)))

(defun trimmed-file-lines (path)
  (mapcar #'str-trim (io-readlines path)))

(defun line-reader-from-path (path &key (remove-test nil))
  (let ((lines (coerce (mapcar #'str-trim (io-readlines path)) 'vector)))
    (if remove-test
        (setf lines (remove-if remove-test lines)))
    (make-instance 'line-reader :lines lines)))

(defun std-line-removes (l)
  (or (str:empty? l)
      (str-starts-with "//" l)
      (str-starts-with "#" l)))


(defun std-file-reader (file-path)
  (line-reader-from-path file-path
                         :remove-test #'std-line-removes))



    
    


         
  
