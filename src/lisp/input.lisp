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


;; WSJ tagging evaluation corpus
(defparameter *wsj-train-file*
  (merge-pathnames "wsj.tt" *eval-path*))

(defparameter *wsj-eval-file*
  (merge-pathnames "test.tt" *eval-path*))

(defvar *normalizer*
    (make-instance 'id-normalizer))

(defvar *wsj-train-corpus* nil)
(defvar *wsj-test-corpus* nil)

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

(defun read-wsj-corpus ()
  (setf *wsj-train-corpus* (read-tt-corpus *wsj-train-file*))
  (setf *wsj-test-corpus* (read-tt-corpus *wsj-eval-file*
                                          :symbol-table *symbol-table*)))
;; Brown corpus
(defvar *brown-train-corpus* nil)
(defvar *brown-eval-corpus* nil)

(defun interleave (list item)
  (rest
   (loop for i in (reverse list)
         appending (list item i))))

(defun read-brown-line (line)
  (let ((items (split-sequence-if #'(lambda (x)
                                      (member x '(#\Space #\Tab)))
                                  line
                                  :remove-empty-subseqs t)))
    (loop for item in items
          ;; Handle cases where the word contains #\/
          collect (let* ((split (position #\/ item :from-end t))
                         (token (subseq item 0 split))
                         (tag (subseq item (1+ split))))
                    (list token tag)))))

(defun read-brown-file (file)
  (with-open-file (s file :direction :input)
    (loop for input  = (read-line s nil nil)
          for line = (or input (string-trim '(#\Space #\Tab #\Newline) input))
          until (null input)
          unless (string-equal "" line)
          collect (read-brown-line line))))

(defun read-brown-corpus (&optional (eval-split 0.1))
  ;; Directory globbing might not work on all systems
  ;; Assumes sorted directory list is returned
  (let* ((brown-glob (merge-pathnames "brown/c*" *eval-path*))
         (brown-files (mapcar #'first
                              (remove-if-not #'(lambda (x)
                                                 (cl-ppcre:scan "^c[a-z]\\d\\d" x))
                                         (sort (loop for file in (directory brown-glob)
                                                     collect (list file (pathname-name file)))
                                               #'string-lessp :key #'second)
                                         :key #'second)))
         (file-split (- (length brown-files)
                        (ceiling (* eval-split (length brown-files)))))
         (train-files (subseq brown-files 0 file-split))
         (eval-files (subseq brown-files file-split)))
    (setf *brown-train-corpus* (loop for file in train-files
                                     appending (read-brown-file file)))
    (setf *brown-eval-corpus* (loop for file in eval-files
                                     appending (read-brown-file file)))))
