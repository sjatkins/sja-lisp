;;;; sja.asd

(asdf:defsystem #:sja
  :description "Describe sja here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:cl-ppcre #:str #:uiop #:yason)
  :components ((:file "package")
               (:file "strings")
               (:file "io")
               (:file "dicts")
               (:file "scanner")
               (:file "bytes")
               (:file "sja")))
