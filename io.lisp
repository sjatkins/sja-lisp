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

(defun file-byte-buffer (file-path)
  (make-instance 'bytes-buffer :bytes (read-file-bytes file-path)))

(defun binary-stream (file-path)
  (open file-path :direction :input :element-type '(unsigned-byte 8)))

(defclass bytes-buffer ()
  ((bytes :accessor bytes :initarg :bytes)
   (pos :accessor pos :initform 0)))

  
                             
(defgeneric get-bytes (buffer sz))
(defgeneric peek-bytes (buffer sz))
(defgeneric read-one-int (buffer &optional (sz 1)  &key (signed '())))
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
  (let ((answer (peek-bytes buffer sz)))
    (incf (pos buffer) sz)
    answer))

(defun set-pos (buffer new-pos)
  (setf (pos buffer) new-pos))

(defun reset-buffer (buffer)
  (set-pos buffer 0))

(defun int-from-bytes (bytes converter)
  (let* ((len (length bytes))
         (array (coerce bytes 'vector)))
    (funcall converter array len)))

(defmethod read-one-int ((buffer bytes-buffer) &optional (sz 1) &key (signed t))
  (let ((bytes (get-bytes buffer sz))
        (converter (if signed #'cl-intbytes:octets->int #'cl-intbytes:octets->uint)))
    (int-from-bytes bytes converter)))

(defun 1-or-more (fn count)
  (if (= count 1)
      (funcall fn)
      (let ((many '()))
        (dotimes (i count)
          (push (funcall fn) many))
        (nreverse many))))

(defmethod read-int ((buffer bytes-buffer) &optional (sz 1) &key (count 1) (signed '()))
  (labels ((read-one ()
             (read-one-int buffer sz :signed signed)))
    (1-or-more #'read-one count)))

(defun u8 (buffer)
  (read-int))

(defun s8 (buffer)
  (read-int :signed t))

(defun u16 (buffer)
  (read-int 2))

(defun s16 (buffer)
  (read-int 2 :signed t))

(defun u32 (buffer)
  (read-int 4))

(defun s32 (buffer)
  (read-int 4 :signed t))



  
