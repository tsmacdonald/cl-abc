;;;; cl-abc.lisp

(in-package #:cl-abc)

;;; "cl-abc" goes here. Hacks and glory await!

(defparameter *config-filename* "config.lisp")

(defun read-config-file ()
  (with-open-file (config *config-filename*
                   :direction :input)
    (defparameter *filenames* (getf (read config) :filenames))))


