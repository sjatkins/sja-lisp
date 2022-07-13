(in-package #:sja)

(defun io-read (file-path)
  (str:from-file file-path))

(defun io-readlines (file-path)
  (uiop:read-file-lines file-path))

(defun read-json (file-path)
  (let ((contents (io-read file-path)))
    (yason:parse contents)))
