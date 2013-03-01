(in-package #:cl-abc)

(defparameter *config-filename* "~/src/cl-abc/config.lisp" "Path of the configuration file")
(defparameter *settings* '(:filenames ()) "Stores necessary settings (and also sets the defaults)")
(defparameter *metainformation-fields* '(area book composer discography file-url group
					 history instruction key unit-note-length meter
					 macro notes origin parts tempo rhythm remark
					 source symbol-line title user-defined voice
					 end-words inline-words reference-number transcription))

(defparameter +note-regex+ "(\\^{0,2}|=?|_{0,2})([A-Ga-g])(,*|'*)([/0-9]*)(<|>)?" ;Should be defconstant, but SBCL is ridiculous
  "Regular expression that's supposed to capture one ABC note, with appropriate affixes")

(defparameter *dottedness* '(1 . 1)
  "Coefficients for the current and next note lengths.")

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
  ((notes :initarg :notes :initform '() :accessor notes))
  (:documentation "Represents a measure of music; contains a sequence of notes."))

(defclass note ()
  ((pitch :initarg :pitch :reader note-pitch)
   (duration :initarg :length :initform 1/8 :reader note-length))
  (:documentation "Represents a note."))

(defclass pitch ()
  ((note :initarg :note :reader pitch-value)
   (accidental :initarg :accidental :initform 'n :reader pitch-accidental)
   (octave :initarg :octave :reader pitch-octave))
  (:documentation "Represents a pitch using scientific pitch notation."))

(defun make-tune ()
  "Creates an empty tune."
  (make-instance 'tune))

(defun headerp (line)
  "Tests if the line is a header or not."
  (when (cl-ppcre:scan "[ABCDFGHIKLMmNOPQRrSsTUVWwXZ]:.*" line)
      line))

(defgeneric add-metainformation (tune line)
  (:documentation "Parses the given line and adds it to the tune."))

(defmethod add-metainformation ((tune tune) (line string))
  (when (headerp line)
    (cl-ppcre:register-groups-bind (field content) ("(.)\\s*:\\s*(.*)" line)
      (case (character field)
	(#\A (setf (tune-area tune) content))
	(#\B (setf (tune-book tune) content))
	(#\C (setf (tune-composer tune) content))
	(#\D (setf (tune-discography tune) content))
	(#\F (setf (tune-file-url tune) content))
	(#\G (setf (tune-group tune) content))
	(#\H (setf (tune-history tune) content))
	(#\I (setf (tune-instruction tune) content))
	(#\K (setf (tune-key tune) content))
	(#\L (setf (tune-unit-note-length tune) (read-from-string content)))
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
	(#\V (setf (tune-voice tune) (parse-integer content :junk-allowed t)))
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

(defmacro remove-from-body (regex line &optional (replacement ""))
  "If the line is part of the tune body, replaces `regex' with
   `replacement'. Otherwise returns the line unchanged."
  `(if-not (headerp ,line)
	   (cl-ppcre:regex-replace-all ,regex ,line ,replacement)
	   ,line))

(defun remove-comments (line)
  "Returns a copy of the line with comments removed."
  (cl-ppcre:regex-replace "%.*" line ""))

(defun remove-whitespace (line)
  "Returns a copy of the line with unnecessary whitespace removed."
  (cl-ppcre:regex-replace "\\s*$" (cl-ppcre:regex-replace "^\\s*" line "") ""))

(defun remove-grace-notes (line)
  "Returns a copy of the line with all gracenotes (and their surrounding braces) removed."
  (remove-from-body "{.*?}" line))

(defun remove-ornaments (line)
  "Returns a copy of the line with all ornaments removed."
  (remove-from-body "(!.*?!)|~" line))

(defun remove-inline-part (line)
  "Returns a copy of the line with all inline second parts (noted by an ampersand) removed."
  (remove-from-body "&.*?\\|" line "|"))

(defun flatten-chords (line)
  "Returns a copy of the line with all chords replaced by their last note."
  (remove-from-body
   (concatenate 'string "\\[(" +note-regex+ ")*?(" +note-regex+ ")\\]")
   line
   "\\7"))

(defun remove-chord-letters (line)
  "Returns a copy of the line with all chord letters (or textual notes) removed."
  (remove-from-body "\".*?\"" line))

(defun clean-line (line)
  "Removes unnecessary whitespace and comments. Also removes unsupported features."
  (when line
    (remove-chord-letters (flatten-chords (remove-ornaments (remove-grace-notes (remove-inline-part (remove-whitespace (remove-comments line)))))))))

(defun parse-body (tune raw-body)
  "Parses the entire musical section of a tune; returns a tune object."
  (format t "~&Parsing tune: {~a}" raw-body)
  (let ((measures (cl-ppcre:split "\\s*\\|+\\s*" raw-body)))
    (setf (tune-melody tune) (mapcar (lambda (ms) (parse-notes tune ms)) measures)))
  tune)

(defun parse-notes (tune measure)
  "Parses a given measure of music; returns a list of notes."
  (format t "~&Parsing measure: {~a}" measure)
  (mapcar (lambda (m) (parse-note tune m)) (cl-ppcre:all-matches-as-strings +note-regex+ measure)))

(defun parse-accidental (accidentals)
  (cond
    ((cl-ppcre:scan "\\^{2}" accidentals) (error "No support for double sharps"))
    ((cl-ppcre:scan "\\^" accidentals) 's)
    ((cl-ppcre:scan "__"  accidentals) (error "No support for double flats"))
    ((cl-ppcre:scan "_" accidentals) 'f)
    ((cl-ppcre:scan "=" accidentals) 'n)))

(defun parse-note (tune note)
  "Parses a given note, returns a note object."
  (format t "~&Parsing note: {~a}" note)
  (unless (cl-ppcre:scan (concatenate 'string "^" +note-regex+ "$") note)
    (error "Invalid note syntax: ~a" note))
  (cl-ppcre:register-groups-bind (prefixes note octave-designator suffixes dotted-indicator)
      (+note-regex+ note)
    (let*
	((starting-octave (if (upper-case-p (char note 0)) 4 5))
	 (designator-length (length octave-designator))
	 (modifier (if-not (zerop designator-length)
			   (* designator-length
			      (if (string-equal (subseq octave-designator 0 1) "'")
				  1
				  -1))
			   0))
	 (octave (+ starting-octave modifier))
	 (note-sym (intern (string-upcase note)))
	 (new-accidental (parse-accidental prefixes))
	 (accidental (or new-accidental (get-accidental-for note-sym (tune-key tune))))
	 (length (* (tune-unit-note-length tune) (if (string-equal suffixes "")
						     1
						     (parse-note-length suffixes)))))
      (setf *dottedness*
	    (cond
	      ((or (null dotted-indicator)
		   (string-equal dotted-indicator ""))
	       (cons (cdr *dottedness*) 1))
	      ((string-equal dotted-indicator ">") '(3/2 . 1/2))
	      ((string-equal dotted-indicator "<") '(1/2 . 3/2))))
      (make-instance 'note :length (* length (car *dottedness*))
		     :pitch (make-instance 'pitch :octave octave
					   :note note-sym :accidental accidental)))))

(defun parse-note-length (raw)
  (if (cl-ppcre:scan "/+" raw)
      (expt 1/2 (length raw))
      (parse-integer raw)))

(defun get-accidental-for (note key)
  (let* ((circle-of-fifths '(C G D A E B F# Db Ab Eb Bb F))
	 (sharps '(F C G D A E B))
	 (flats (reverse sharps)))
    (cl-ppcre:register-groups-bind (raw-key mode)
	("([A-G][#b]?)(m?)" key)
      (let* ((sharp-count (position (intern (string-upcase raw-key)) circle-of-fifths))
	     (minorp (string-equal mode "m"))
	     (flatp (or (> sharp-count 6) (and minorp (< sharp-count 3))))
	     (accidental-list (if flatp flats sharps))
	     (accidental-count (funcall (if flatp #'+ #'- )
					(if-not flatp sharp-count (mod (- 12 sharp-count) 12))
					(if minorp 3 0)))
	     (note-position (position note accidental-list)))
	(if (>= note-position accidental-count)
	    'n
	    (if flatp 'f 's))))))
	     
(defun parse-file (filename)
  "Main entry point to parse a given file. Returns a list of tune objects."
  (let ((tunes '())
	(tune (make-tune))
	(body "")
	(skipping nil))
    (with-open-file (file filename
			  :direction :input)
      (loop for line = (clean-line (read-line file nil))
	 while line do
	   (format t "~&Parsing line: {~a}~&" line)
	   (if (headerp line)
	       (progn
		 (when (and (cl-ppcre:scan "\\s*X\\s*:" line) (string-not-equal body ""))
		   (push (parse-body tune body) tunes)
		   (setq tune (make-tune))
		   (setq body "")
		   (setq skipping nil))
		 (format t "Skipping: ~a" skipping)
		 (unless skipping (add-metainformation tune line))
		 (when (> (or (tune-voice tune) 0) 1)
		     (setq skipping t)))
	       (unless skipping (setq body (concatenate 'string body line))))))
    (format t "~&L: ~a" (tune-unit-note-length tune))
    (push (parse-body tune body) tunes)
    (nreverse tunes)))


(defun print-note (note)
  (format nil "~a~a[~a]" (pitch-value (note-pitch note)) (pitch-octave (note-pitch note)) (note-length note)))

(defun print-tune (tune)
  "Quick hack to print a tune"
  (format t "~&~{~{~a~^ ~}~^ | ~}" (mapcar (lambda (x) (mapcar #'print-note x)) (tune-melody tune))))

(defun make-sample-tune ()
  (make-instance 'tune :unit-note-length 1/8 :title "Foobar" :key "C"))