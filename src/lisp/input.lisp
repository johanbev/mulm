(in-package :mulm)

(defgeneric tokenize (token tokenizer))
(defgeneric normalize (token normalizer))

(defclass tokenizer nil nil)

(defclass normalizer nil nil)
(defclass downcasing (normalizer) nil)
(defclass id-normalizer (normalizer) nil)
(defclass cd-normalizer (normalizer) nil)

;; Lispworks doesn't do unicode char names
#-lispworks
(defparameter *illegal-token-chars*
    '(#\: #\, #\. #\- #\/ #\% #\\ #\#
      #\CURRENCY_SIGN
      #\$
      #\POUND_SIGN
      #\EURO_SIGN
      #\' #\` #\( #\) #\;))

#+lispworks
(defparameter *illegal-token-chars*
    '(#\: #\, #\. #\- #\/ #\% #\\ #\#
      #\U+00A4 ;; CURRENCY SIGN
      #\$
      #\U+00A3 ;; POUND SIGN
      #\U+29AC ;; EURO SIGN
      #\' #\` #\( #\) #\;))

(defmethod normalize (token (normalizer downcasing))
  (string-downcase token))

(defmethod normalize  (token (normalizer id-normalizer))
  token)

(defmethod normalize (token (normalizer cd-normalizer))
  (let ((tok (remove-if (lambda (x) (find x *illegal-token-chars*))
                        (string-downcase token))))
    (if (string= tok "")
      token
      (if (or (member tok '("one" "two" "three" "four" "five" "six" "seven" "ten" "twenty") :test #'string=)
              ;; the CL reader doesn't like these two characters which appear in the OBT corpora
              (and (not (find-if #'(lambda (c)
                                     (member c '(#\| #\")))
                                 tok))
                   ;; intern arbitrary strings into the keyword package
                   (let ((*package* (find-package :keyword)))
                     (numberp (read-from-string tok)))))
        (prog1 
            (if (eql #\. (aref token (1- (length token))))
              "|cd|"
              "|od|"))
        token))))

(defvar *normalizer*
    (make-instance 'cd-normalizer))


; WSJ tagging evaluation corpus
(defparameter *wsj-train-file*
  (merge-pathnames "wsj/wsj.tt" *eval-path*))

(defparameter *wsj-eval-file*
    (merge-pathnames "wsj/test.tt" *eval-path*))
(defvar *wsj-train-corpus* nil)
(defvar *wsj-test-corpus* nil)

(defun normalize-token (token)
  (normalize token *normalizer*))

(defun split-tag-constraint (tag)
  (let ((tags (cl-ppcre:all-matches-as-strings "[^\|\\s]+" tag)))
    (list (first tags) (rest tags))))

(defun read-tt-corpus (file &key lexicon (constrained nil))
  "Create a list of lists corpus from TT format file."
  (declare (ignore lexicon))
  (with-open-file (stream file :direction :input)
    (loop with forms
          for line = (read-line stream nil)
          ;; split on any whitespace
          for split = (position-if #'(lambda (c)
                                       (member c *whitespace*)) line)
          for form = (string-trim *whitespace* (normalize-token (subseq line 0 split)))
          for tag = (if split (string-trim *whitespace* (subseq line (+ split 1))))
          while line
          when (and form tag (not (string= form "")))
          do (if constrained
               (destructuring-bind (tag constraint) (split-tag-constraint tag)
                 (push (list form tag constraint) forms))
               (push (list form tag) forms))
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

(defun ll-to-constraint-list (ll)
  (loop for seq in ll
        collect (loop for tok in seq
                      collect (third tok))))

(defun read-wsj-corpus ()
  (setf *wsj-train-corpus* (read-tt-corpus *wsj-train-file*))
  (setf *wsj-test-corpus* (read-tt-corpus *wsj-eval-file*)))
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
                         (token (normalize-token (subseq item 0 split)))
                         (tag (destructure-brown-tag (subseq item (1+ split)))))
                    (list token tag)))))

(defun destructure-brown-tag (tag)
  (unless (string= "" (string-trim "*" tag))
    (setf tag (string-trim "*" tag)))
  (let* ((parts (cl-ppcre:split "-|\\+" tag)))
    (cond
     ((or (null parts) (string= "" (first parts)))
      (if (string= "---hl" tag)
	  "--"
	tag))
     ((string= "FW" (first parts))
	(or (second parts) (first parts)))
     (t (first parts)))))

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

(defun write-tt-file (path corpus)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (loop
     for sequence in corpus
     do (loop
	       for (form tag) in sequence
	       do (format stream "~a~c~a~%" form #\Tab tag))
	   (format stream "~%"))))
