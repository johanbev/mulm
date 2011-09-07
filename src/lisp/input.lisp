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

(defparameter *number-scanner*
    (cl-ppcre:create-scanner "four|five|six|seven|eight|nine|ten|twenty|million|billion"))

(defparameter *digit-scanner-string*
  (format nil "^[$~a~a]?[0-9]+[0-9,:/]*([\\.,][0-9]+)?[$~a~a]?$"
          (code-char #x20AC) ; euro sign
          (code-char #x00A3) ; pound sign
          (code-char #x20AC) ; euro sign
          (code-char #x00A3) ; pound sign
          ))

(defparameter *digit-scanner*
    (cl-ppcre:create-scanner *digit-scanner-string*))

(defun string-numberp (token)
  (funcall *digit-scanner* token 0 (length token)))


(defmethod normalize (token (normalizer cd-normalizer))
  (if (string-numberp token)
      (if (eql #\. (aref token (1- (length token))))
          "|od|"
        "|cd|")
    token))

(defvar *normalizer*
    (make-instance 'cd-normalizer))

(defvar *tag-map* nil)

(defun read-tag-map (file)
  (with-open-file (stream file :direction :input)
    (loop 
        with tag-map = (make-hash-table :test #'equal)
        for line = (read-line stream nil)
                     ;; split on any whitespace
        for split = (position-if #'(lambda (c)
                                     (member c *whitespace*)) line)
        for from = (string-trim *whitespace* (normalize-token (subseq line 0 split)))
        for to = (if split (string-trim *whitespace* (subseq line (+ split 1))))
        while line
        do (setf (gethash from tag-map) to)
        finally (return tag-map))))

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

(defun read-tt-corpus (file &key lexicon (constrained nil) (tag-map nil))
  "Create a list of lists corpus from TT format file."
  (declare (ignore lexicon))
  (let ((result nil)
        (token-count 0))

    (log5:log-for (log5:info) "Reading corpus from file ~a" file)
  
    (with-open-file (stream file :direction :input)
      (loop 
       with accu 
       with forms
       for line = (read-line stream nil)
       ;; split on any whitespace
       for split = (position-if #'(lambda (c)
                                    (member c *whitespace*)) line)
       for form = (string-trim *whitespace* (normalize-token (subseq line 0 split)))
       for raw-tag = (if split (string-trim *whitespace* (subseq line (+ split 1))))
       for tag = (if tag-map
                   (gethash raw-tag tag-map raw-tag)
                   raw-tag)                  
       while line
       if (and form (not (string= form "")))
       do (if constrained
            (destructuring-bind (tag constraint) (split-tag-constraint tag)
              (push (list form tag constraint) forms))
            (push (list form tag) forms))
       and do (incf token-count)
       else do
       (when forms
         (push (nreverse forms)  accu))
       (setf forms nil)
       finally (when forms (push (nreverse forms) accu)) (setf result (nreverse accu))))

    (log5:log-for (log5:info) "Read ~a tokens in training corpus" token-count)

    result))


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


(defun check-cd-mapping (corpus &key (cd-tag "CD"))
  (loop for sent in corpus
      nconcing
        (loop for (word tag) in sent
            with it
            do
              (cond
               ((and (string= tag cd-tag) (not (or (string= word "|cd|") (string= word "|od|"))))
                (push (list word tag) it))
               ((and (or (string= word "|cd|") (string= word "|od|"))
                     (not (string= tag cd-tag)))
                (push (list word tag) it)))
            finally (return it))))

(iterate:defmacro-driver (iterate:FOR sentence IN-CORPUS-STREAM corpus-stream)
  (let ((kwd (if iterate:generate 'iterate:generate 'iterate:for)))
    `(progn
       (,kwd ,sentence iterate:next
             (progn
               (let ((sentence (loop for line = (read-line ,corpus-stream nil nil)
                                     while line
                                     until (string-equal (string-trim *whitespace* line) "")
                                     collect line)))
                 (if sentence
                   sentence
                   (iterate:terminate))))))))


