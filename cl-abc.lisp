(in-package #:cl-abc)

(defparameter *config-filename* "~/src/cl-abc/config.lisp" "Path of the configuration file") ;;FIXME!
(defparameter *settings* '(:filenames ()) "Stores necessary settings (and also sets the defaults)")
(defparameter *metainformation-fields* '(area book composer discography file-url group
					 history instruction key unit-note-length meter
					 macro notes origin parts tempo rhythm remark
					 source symbol-line title user-defined voice
					 end-words inline-words reference-number transcription))

;Should be defconstant, but SBCL is ridiculous
(defparameter +note-regex+ "[\\^=_]*[A-Ga-g][,']*[/0-9]*"
  "Regular expression that's supposed to capture one ABC note, with appropriate affixes")

(defmacro if-not (test then &optional else)
  `(if (not ,test) ,then ,else))

(let ((*metainformation-slot-forms* 
       (loop for property in *metainformation-fields*
	  collecting (list property
			   :initarg (intern (symbol-name property) :keyword)
			   :initform nil
			   :accessor (values
				      (intern
				       (string-upcase (concatenate 'string "tune-"
								   (symbol-name property)))))))))
  (eval
   `(defclass tune ()
      (,@*metainformation-slot-forms*
       (melody :initarg :melody :initform '() :accessor tune-melody))
      (:documentation "Represents a tune, with metainformation and a list of its measures"))))

(defclass measure ()
  ((notes :initarg :notes :initform '() :accessor notes)))

(defclass note ()
  ((pitch :initarg :pitch :reader note-pitch)
   (duration :initarg :duration :initform 1/8 :reader note-duration))
  (:documentation "Represents a note."))

(defclass pitch ()
  ((note :initarg :note :reader pitch-value)
   (accidental :initarg :accidental :initform 'n :reader pitch-accidental)
   (octave :initarg :octave :reader pitch-octave)))


(defun make-tune ()
  "Creates an empty tune."
  (make-instance 'tune))


(defun headerp (line)
  (when (cl-ppcre:scan "[ABCDFGHIKLMmNOPQRrSsTUVWwXZ]:.*" line)
      line))

(defgeneric add-metainformation (tune line)
  (:documentation "Parses the given line and adds it to the tune."))

(defmethod add-metainformation ((tune tune) (line string))
  (when (headerp line)
    (cl-ppcre:register-groups-bind (field content) ("(.)\\s*:\\s*(.*)" line)
      (case (coerce field 'character)
	(#\A (setf (tune-area tune) content))
	(#\B (setf (tune-book tune) content))
	(#\C (setf (tune-composer tune) content))
	(#\D (setf (tune-discography tune) content))
	(#\F (setf (tune-file-url tune) content))
	(#\G (setf (tune-group tune) content))
	(#\H (setf (tune-history tune) content))
	(#\I (setf (tune-instruction tune) content))
	(#\K (setf (tune-key tune) content))
	(#\L (setf (tune-unit-note-length tune) content))
	(#\M (setf (tune-meter tune) content))
	(#\m (setf (tune-macro tune) content))
	(#\N (setf (tune-notes tune) content))
	(#\O (setf (tune-origin tune) content))
	(#\P (setf (tune-parts tune) content))
	(#\Q (setf (tune-tempo tune) content))
	(#\R (setf (tune-rhythm tune) content))
	(#\r (setf (tune-remark tune) content))
	(#\S (setf (tune-source tune) content))
	(#\s (setf (tune-symbol-line tune) content))
	(#\T (setf (tune-title tune) content))
	(#\U (setf (tune-user-defined tune) content))
	(#\V (setf (tune-voice tune) content))
	(#\W (setf (tune-end-words tune) content))
	(#\w (setf (tune-inline-words tune) content))
	(#\X (setf (tune-reference-number tune) content))
	(#\Z (setf (tune-transcription tune) content))))))

(defgeneric add-melody-notes (tune note-seq)
  (:documentation "Adds the given note sequence to the end of the tune."))

(defmethod add-melody-notes ((tune tune) note-seq)
  (setf (tune-melody tune) (append (tune-melody tune) note-seq)))

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


(defun parse-body (tune raw-body)
  (format t "~&Parsing tune: {~a}" raw-body)
  (let ((measures (cl-ppcre:split "\\s*\\|+\\s*" raw-body)))
    (setf (tune-melody tune) (mapcar (lambda (ms) (parse-notes tune ms)) measures)))
  tune)

(defun parse-notes (tune measure)
  (format t "~&Parsing measure: {~a}" measure)
  (mapcar (lambda (m) (parse-note tune m)) (cl-ppcre:all-matches-as-strings +note-regex+ measure)))

(defun parse-note (tune note)
  (format t "~&Parsing note: {~a}" note)
  (cl-ppcre:register-groups-bind (prefixes note octave-designator suffixes)
      ("(.*)([A-Ga-g])([,']*)(.*)" note) ;; FIXME
    (let* ((starting-octave (if (upper-case-p (char note 0)) 4 5))
	   (designator-length (length octave-designator))
	   (modifier (if-not (zerop designator-length)
	     (* designator-length
		(if (string-equal (subseq octave-designator 0 1) "'")
		    1
		    -1))
	     0))
	  (octave (+ starting-octave modifier)))
      (make-instance 'note :pitch (make-instance 'pitch :octave octave :note (string-upcase note))))))

(defun parse-file (filename)
  "Main entry point to parse a given file. Returns a TUNE item."
  (let ((tune (make-tune))
	(body ""))
    (with-open-file (file filename
			  :direction :input)
      (loop for line = (clean-line (read-line file nil))
	 while line do
	   (format t "Parsing {~a}~&" line)
	   (if (headerp line)
	       (add-metainformation tune line)
	       (setq body (concatenate 'string body line)))))
    (parse-body tune body)))
	   
