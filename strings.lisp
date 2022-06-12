(in-package #:sja)

(defun str-trim (string)
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun str-in (sub s)
  (search sub s))

(defun str-split-trim (split seq)
  (mapcar #'str-trim (str:split split seq :omit-nulls t)))

(defun str-starts-with (pred str)
  (alexandria::starts-with-subseq pred str))

(defun str-ends-with (pred str)
  (alexandria::ends-with-subseq pred str))

(defun str-find (substr str)
  (search substr str))

(defun str-replace (substr replacement str)
  (str:replace-all substr replacement str))



