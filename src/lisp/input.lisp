(in-package :mulm)

(defgeneric tokenize (token tokenizer))
(defgeneric normalize (token normalizer))

(defclass tokenizer nil nil)

(defclass normalizer nil nil)
(defclass downcasing (normalizer) nil)
(defclass id-normalizer (normalizer) nil)

(defmethod normalize (token (normalizer downcasing))
  (string-downcase token))

(defmethod normalize  (token (normalizer id-normalizer))
  token)

(defvar *normalizer*
    (make-instance 'id-normalizer))

(defun normalize-token (token)
  (normalize token *normalizer*))

(defun read-tt-corpus (file)
  "Create a list of lists corpus from TT format file."
  (with-open-file (stream file :direction :input)
    (loop
	with forms
	for line = (read-line stream nil)
        for tab = (position #\tab line)
        for form = (normalize-token (subseq line 0 tab))
	for code = (symbol-to-code form)
        for tag = (if tab (subseq line (+ tab 1)))
	while line
        when (and form tag (not (string= form ""))) do
          (push (list code tag) forms)
	else collect (nreverse forms) and do (setf forms nil))))
