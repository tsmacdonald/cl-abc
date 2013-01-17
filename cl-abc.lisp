(in-package #:cl-abc)

(defparameter *config-filename* "~/src/cl-abc/config.lisp") ;;FIXME!
(defparameter *settings* '(:filenames ()))
(defparameter *metainformation-fields* '(area book composer discography file-url group
					 history instruction key unit-note-length meter
					 macro notes origin parts tempo rhythm remark
					 source symbol-line title user-defined voice
					 end-words inline-words reference-number transcription))
(defun read-config-file ()
  "Opens the configuration files and sets relevant variables."
  (with-open-file (config *config-filename*
			  :direction :input
			  :if-does-not-exist nil)
    (when config (setq *settings* (read config)))))

(defun remove-comments (line)
  (cl-ppcre:regex-replace "%.*" line ""))

(defun remove-whitespace (line)
  (cl-ppcre:regex-replace "\\s*$" (cl-ppcre:regex-replace "^\\s*" line "") ""))

(defun clean-line (line)
  "Removes unnecessary whitespace and comments."
  (when line
    (remove-whitespace (remove-comments line))))

(defun headerp (line)
  (when (cl-ppcre:scan "[ABCDFGHIKLMmNOPQRrSsTUVWwXZ]:.*" line)
      line))

(defun parse-file (filename)
  "Main entry point to parse a given file. Returns a TUNE item."
  (let ((metainformation (make-metainformation))
	(body (make-body)))
    (with-open-file (file filename
			  :direction :input)
      (loop for line = (clean-line (read-line file nil))
	 while line do
	   (format t "Parsing {~a}~&" line)
	   (if (headerp line)
	       (add-metainformation metainformation line)
	       (add-notes body (parse-body line)))))
    (make-tune metainformation body)))

(eval
 `(defclass metainformation ()
	 ,(loop for property in *metainformation-fields*
		collecting (list property
				 :initarg (intern (symbol-name property) :keyword)
				 :accessor (values (intern (string-upcase (concatenate 'string "tune-"
								(symbol-name property)))))))
	 (:documentation "Describes the metainformation (title, key signature, etc.) of a tune")))