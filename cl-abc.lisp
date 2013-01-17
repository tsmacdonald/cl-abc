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

(defun make-tune ()
  (make-instance 'tune))

(defgeneric add-metainformation (tune line)
  (:documentation "Parses the given line and adds it to the tune"))

(defmethod add-metainformation ((tune tune) (line string))
  ;;;hahahahahahhaa
  nil)

(defgeneric add-melody-notes (tune note-seq)
  (:documentation "Adds the given note sequence to the end of the tune."))

(defmethod add-melody-notes ((tune tune) note-seq)
  (setf (tune-melody tune) (append (tune-melody tune) note-seq)))

(defun parse-body (line) ;; FIXME
  (list (make-instance 'note :value 'C :octave 4) (make-instance 'note :value 'D :octave 4)))

(defun parse-file (filename)
  "Main entry point to parse a given file. Returns a TUNE item."
  (let ((tune (make-tune)))
    (with-open-file (file filename
			  :direction :input)
      (loop for line = (clean-line (read-line file nil))
	 while line do
	   (format t "Parsing {~a}~&" line)
	   (if (headerp line)
	       (add-metainformation tune line)
	       (add-melody-notes tune (parse-body line)))))
    tune))
	   

(defparameter *metainformation-slot-forms* 
  (loop for property in *metainformation-fields*
     collecting (list property
		      :initarg (intern (symbol-name property) :keyword)
		      :accessor (values (intern (string-upcase (concatenate 'string "tune-"
									    (symbol-name property))))))))
(eval
 `(defclass tune ()
    (,@*metainformation-slot-forms*
     (melody :initarg :melody :initform '() :accessor tune-melody))
    (:documentation "Fooooooo")))

(defclass note ()
  ((value :initarg :value :reader note-value)
   (octave :initarg :octave :reader note-octave)
   (accidental :initarg :accidental :initform 'natural :reader note-accidental)
   (duration :initarg :duration :initform 1/8 :reader note-duration))
  (:documentation "Represents a note."))