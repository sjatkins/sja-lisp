(in-package #:sja)

(defun io-read (file-path)
  (str:from-file file-path))

(defun io-readlines (file-path)
  (uiop:read-file-lines file-path))

(defun read-json (file-path)
  (let ((contents (io-read file-path)))
    (yason:parse contents)))

(defun read-file-bytes (file-path)
  (with-open-file (stream file-path  :direction :input :element-type '(unsigned-byte 8))
    (let* ((sz (file-length stream))
           (contents (make-array sz :element-type '(unsigned-byte 8))))
      (read-sequence contents stream)
      contents)))

(defun binary-stream (file-path)
  (open file-path :direction :input :element-type '(unsigned-byte 8)))

(defclass bytes-buffer ()
  ((bytes :accessor bytes :initarg :bytes)
   (pos :accessor pos :initform 0)))

  
                             
(defgeneric get-bytes (buffer sz))
(defgeneric peek-bytes (buffer sz))
(defgeneric read-one-int (buffer &key))
(defgeneric read-string (buffer))

(defmethod read-string ((buffer bytes-buffer))
  (let* ((count (read-one-int buffer :sz 2))
        (bytes (get-bytes buffer count))
        (chars (mapcar #'code-char bytes)))
    (coerce (nreverse (cdr (nreverse chars))) 'string)))

(defmethod peek-bytes ((buffer bytes-buffer) sz)
  (let ((buf '()))
    (dotimes (i sz)
      (push (aref (bytes buffer) (+ (pos buffer) i)) buf))
    (nreverse buf)))

(defmethod get-bytes ((buffer bytes-buffer) sz)
  (let ((answer (peak-bytes buffer sz)))
    (incf (pos buffer) sz)
    answer))

(defun set-pos (buffer new-pos)
  (setf (pos buffer) new-pos))

(defun reset-buffer (buffer)
  (set-pos buffer 0))

(defun int-from-bytes (bytes)
  (let* ((len (length bytes))
         (mult (mapcar (lambda (i) (ash 1 (* 8 i))) (alexandria:iota len))))
    (apply #'+ (mapcar (lambda (a b) (* a b)) bytes mult))))

(defmethod read-one-int ((buffer bytes-buffer) &key (sz 1))
  (let ((bytes (get-bytes buffer sz)))
    (int-from-bytes bytes)))

  
