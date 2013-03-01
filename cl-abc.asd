;;;; cl-abc.asd

(asdf:defsystem #:cl-abc
  :serial t
  :description "A library that provides a CLOS API for parsing, manipulating, and producing files in ABC notation."
  :author "Tim Macdonald <tsmacdonald@gmail.com>"
  :license "MIT License"
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "cl-abc")))

