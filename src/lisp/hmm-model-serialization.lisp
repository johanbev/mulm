(in-package :mulm)

;; Serialization
(defun serialize-hmm-model-header (hmm s)
  (format s "hmm header n ~a token-count ~a~%" (hmm-n hmm) (hmm-token-count hmm)))

(defun serialize-hmm-emissions (hmm s)
  (format s "hmm emissions start~%")
  (loop for map across (hmm-emissions hmm)
        for i from 0
        do (loop for j being the hash-keys of map
                   for val being the hash-values of map
                   do (format s "~a ~a ~S~%" i j val)))
  (format s "hmm emissions end~%"))

(defun serialize-hmm-unigram-counts (hmm s)
  (format s "hmm unigram counts start~%")
  (loop for i from 0 below (hmm-tag-cardinality hmm)
        do (format s "~a ~S~%"
                   i (aref (hmm-unigram-counts hmm) i)))
  (format s "hmm unigram counts end~%"))

(defun serialize-hmm-bigram-counts (hmm s)
  (let ((c (hmm-tag-cardinality hmm)))
    (format s "hmm bigram counts start~%")
    (loop for i from 0 below c 
          do (loop for j from 0 below c
                   do (format s "~a ~a ~S~%"
                              i j (aref (hmm-bigram-counts hmm) i j))))
    (format s "hmm bigram counts end~%")))

(defun serialize-hmm-trigram-counts (hmm s)
  (let ((c (hmm-tag-cardinality hmm)))
    (format s "hmm trigram counts start~%")
    (loop for i from 0 below c
          do (loop for j from 0 below c
                   do (loop for k from 0 below c
                            when (aref (hmm-trigram-counts hmm) i j k)
                            do (format s "~a ~a ~a ~S~%"
                                       i j k
                                       (aref (hmm-trigram-counts hmm) i j k)))))
    (format s "hmm trigram counts end~%")))

(defun serialize-hmm-model (hmm s)
  (serialize-hmm-model-header hmm s)
  (serialize-lexicon (hmm-tag-lexicon hmm) s :hmm-tag-lexicon)
  (serialize-lexicon (hmm-token-lexicon hmm) s :hmm-token-lexicon)
  (serialize-hmm-emissions hmm s)
  (serialize-hmm-unigram-counts hmm s)
  (serialize-hmm-bigram-counts hmm s)
  (serialize-hmm-trigram-counts hmm s))

(defun serialize-hmm-model-to-file (hmm file &key (if-exists :supersede))
  (with-open-file (s file :direction :output :if-exists if-exists)
    (serialize-hmm-model hmm s)))

;; Deserialization
(defun deserialize-hmm-header (hmm header)
  (let ((tokens (cl-ppcre:all-matches-as-strings "\\S+" header)))
    (unless (equalp (subseq tokens 0 2) '("hmm" "header"))
      (error "HMM model can not be deserialized"))
    (let* ((header (list-to-plist (rest (rest tokens))))
           (n (parse-integer (getf header :n)))
           (token-count (parse-integer (getf header :token-count))))
      (setf (hmm-n hmm) n)
      (setf (hmm-token-count hmm) token-count))
    hmm))

(defun deserialize-hmm-emissions (hmm s)
  (let ((emissions (make-array (hmm-tag-cardinality hmm) :initial-element nil)))
    (unless (equalp (read-line s nil nil)
                    "hmm emissions start")
      (error "HMM model emission table can not be deserialized"))
    (loop for i from 0 to (- (hmm-tag-cardinality hmm) 1)
          do (setf (aref emissions i) (make-hash-table)))
    (loop for line = (read-line s nil nil)
          until (equalp (string-trim *whitespace* line) "hmm emissions end")

          when (null line)
          do (error "Premature end of file")

          do (destructuring-bind (i j value)
                 (cl-ppcre:all-matches-as-strings "\\S+" (string-trim *whitespace* line))
               (let* ((i (parse-integer i))
                      (j (if (equalp j "unk") ;; may be :unk
                           :unk
                           (read-from-string j)))
                      (value (read-from-string value)))
                 (setf (gethash j (aref emissions i)) value))))
    emissions))

(defun deserialize-hmm-unigram-counts (hmm s)
  (let ((unigram-counts (make-array (hmm-tag-cardinality hmm)
                                    :initial-element nil)))
    (unless (equalp (read-line s nil nil)
                    "hmm unigram counts start")
      (error "HMM model unigram counts can not be deserialized"))
    (loop for line = (read-line s nil nil)
          until (equalp (string-trim *whitespace* line) "hmm unigram counts end")

          when (null line)
          do (error "Premature end of file")

          do (destructuring-bind (i value)
                 (cl-ppcre:all-matches-as-strings "\\S+" (string-trim *whitespace* line))
               (setf (aref unigram-counts
                           (parse-integer i))
                     (parse-integer value))))
    unigram-counts))

(defun deserialize-hmm-bigram-counts (hmm s)
  (let* ((c (hmm-tag-cardinality hmm))
         (bigram-counts (make-array (list c c)
                                    :initial-element nil)))
    (unless (equalp (read-line s nil nil)
                    "hmm bigram counts start")
      (error "HMM model bigram counts can not be deserialized"))
    (loop for line = (read-line s nil nil)
          until (equalp (string-trim *whitespace* line) "hmm bigram counts end")

          when (null line)
          do (error "Premature end of file")

          do (destructuring-bind (i j value)
                 (cl-ppcre:all-matches-as-strings "\\S+" (string-trim *whitespace* line))
               (setf (aref bigram-counts
                           (parse-integer i)
                           (parse-integer j))
                     (parse-integer value))))
    bigram-counts))

(defun deserialize-hmm-trigram-counts (hmm s)
  (let* ((c (hmm-tag-cardinality hmm))
         (trigram-counts (make-array (list c c c)
                                     :initial-element nil)))
    (unless (equalp (read-line s nil nil)
                    "hmm trigram counts start")
      (error "HMM model trigram counts can not be deserialized"))
    (loop for line = (read-line s nil nil)
          until (equalp (string-trim *whitespace* line) "hmm trigram counts end")

          when (null line)
          do (error "Premature end of file")

          do (destructuring-bind (i j k value)
                 (cl-ppcre:all-matches-as-strings "\\S+" (string-trim *whitespace* line))
               (setf (aref trigram-counts
                           (parse-integer i)
                           (parse-integer j)
                           (parse-integer k))
                     (parse-integer value))))
    trigram-counts))

(defun deserialize-hmm-model (s)
  (with-standard-io-syntax
    (let ((*package* (find-package :mulm))
          (*read-eval* nil)
          (hmm (make-hmm)))
      (deserialize-hmm-header hmm (read-line s nil nil))
      (setup-hmm hmm (hmm-n hmm) t)
      (setf (hmm-tag-lexicon hmm)
            (second (deserialize-lexicon s :hmm-tag-lexicon)))
      (setf (hmm-token-lexicon hmm)
            (second (deserialize-lexicon s :hmm-token-lexicon)))
      (setf (hmm-emissions hmm)
            (deserialize-hmm-emissions hmm s))
      (setf (hmm-unigram-counts hmm)
            (deserialize-hmm-unigram-counts hmm s))
      (setf (hmm-bigram-counts hmm)
            (deserialize-hmm-bigram-counts hmm s))
      (setf (hmm-trigram-counts hmm)
            (deserialize-hmm-trigram-counts hmm s))


      (calculate-parameters hmm)
      (setup-hmm-beam hmm)

      hmm)))

(defun deserialize-hmm-model-from-file (file)
  (log5:log-for (log5:info) "Reading HMM model from ~a" file)
  (with-open-file (s file)
    (deserialize-hmm-model s)))
