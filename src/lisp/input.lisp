(in-package :mulm)

(defgeneric tokenize (token tokenizer))
(defgeneric normalize (token normalizer))

(defclass tokenizer nil nil)

(defclass normalizer nil nil)
(defclass downcasing (normalizer) nil)
(defclass id-normalizer (normalizer) nil)
(defclass cd-normalizer (normalizer) nil)

(defmethod normalize (token (normalizer downcasing))
  (string-downcase token))

(defmethod normalize  (token (normalizer id-normalizer))
  token)

(defmethod normalize (token (normalizer cd-normalizer))
  (let ((tok (remove-if (lambda (x) (find x ":,.-/%\\#¤$£€'`()#:;"))
				(string-downcase token))))
    (if (string= "" tok)
	token
      (if (or (member tok '("one" "two" "three" "four" "five" "six" "seven" "ten" "twenty") :test #'string=)
	      (numberp (read-from-string tok)))
	  (prog1 "¦CD¦" )
	token))))

(defvar *normalizer*
    (make-instance 'id-normalizer))

(defvar *train-corpus* nil)

(defvar *test-corpus* nil)

(defun normalize-token (token)
  (normalize token *normalizer*))

(defun read-tt-corpus (file &key symbol-table)
  "Create a list of lists corpus from TT format file."
  (with-open-file (stream file :direction :input)
    (loop
	with forms
	for line = (read-line stream nil)
        for tab = (position #\tab line)
        for form = (normalize-token (subseq line 0 tab))
	for code = (if symbol-table
		       (or (symbol-to-code form symbol-table :rop t)
			   :unk)
		     (symbol-to-code form))
        for tag = (if tab (subseq line (+ tab 1)))
	while line
        when (and form tag (not (string= form ""))) do
          (push (list code tag) forms)
	else collect (nreverse forms) and do (setf forms nil))))

(defun ll-to-word-list (ll)
  "Extract the sentences out of a list of lists corpus"
  (mapcar (lambda (x)
	    (mapcar #'car x))
	  ll))

(defun ll-to-tag-list (ll)
  "Extract the tag-sequences out of a list of lists corpus"
  (mapcar (lambda (x)
	     (mapcar #'second x))
	  ll))

(defun read-all-corpora ()
  (setf *train-corpus* (read-tt-corpus *tagger-train-file*))
  (setf *test-corpus* (read-tt-corpus *tagger-eval-file*
                                      :symbol-table *symbol-table*)))
