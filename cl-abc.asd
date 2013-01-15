;;;; cl-abc.asd

(asdf:defsystem #:cl-abc
  :serial t
  :description "Describe cl-abc here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "cl-abc")))

