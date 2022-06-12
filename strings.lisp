(in-package #:sja)

(defun str-trim (string)
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun str-split-trim (split seq)
  (let ((len (length split))
        (res)
        (pos (search split seq)))
    (labels ((split (sequence)
               (let ((pos (search split sequence)))
                 (if pos
                     (progn
                       (push (subseq sequence 0 pos) res)
                       (split (subseq sequence (+ pos len))))
                     (push sequence res)))))
      (split seq)
      (let ((trimmed (mapcar #'str-trim  (nreverse res))))
        (remove-if-not  (lambda (part) (> (length part) 0)) trimmed)))))

