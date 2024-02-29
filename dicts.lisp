(in-package #:sja)

(defun dict-keys (ht) (alexandria:hash-table-keys ht))
(defun dict-values (ht) (alexandria:hash-table-values ht))
(defun dict-map-keys (fn ht) (alexandria:maphash-keys fn ht))
(defun dict-map-values (fn ht) (alexandria:maphash-values fn ht))
(defun dict-get (key ht &optional default)
  (multiple-value-bind (val present) (gethash key ht)
    (if present val default)))


