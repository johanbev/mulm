(in-package :mulm)

;; WARNING serialization and deserialization is currently inaccurate

;; Serialization
(defun serialize-hmm-model-header (hmm s)
  (format s "hmm header n ~a token-count ~a~%" (hmm-tag-cardinality hmm) (hmm-token-count hmm)))

(defun serialize-hmm-transitions (hmm s)
  (format s "hmm transitions start~%")
  (loop for i from 0 below (hmm-tag-cardinality hmm)
        do (loop for j from 0 below (hmm-tag-cardinality hmm)
                 do (format s "~a ~a ~S~%"
                            i j (aref (hmm-transitions hmm) i j))))
  (format s "hmm transitions end~%"))

(defun serialize-hmm-emissions (hmm s)
  (format s "hmm emissions start~%")
  (loop for map across (hmm-emissions hmm)
        for i from 0
        do (loop for j being the hash-keys of map
                   for val being the hash-values of map
                   do (format s "~a ~a ~S~%" i j val)))
  (format s "hmm emissions end~%"))

(defun serialize-hmm-trigram-table (hmm s)
  (format s "hmm trigram table start~%")
  (loop for i from 0 below (hmm-tag-cardinality hmm)
        do (loop for j from 0 below (hmm-tag-cardinality hmm)
                 do (loop for k from 0 below (hmm-tag-cardinality hmm)
                          when (aref (hmm-trigram-table hmm) i j k)
                          do (format s "~a ~a ~a ~S~%"
                                     i j k
                                     (aref (hmm-trigram-table hmm) i j k)))))
  (format s "hmm trigram table end~%"))

(defun serialize-hmm-unigram-table (hmm s)
  (format s "hmm unigram table start~%")
  (loop for i from 0 below (hmm-tag-cardinality hmm)
        do (format s "~a ~S~%"
                   i (aref (hmm-unigram-table hmm) i)))
  (format s "hmm unigram table end~%"))

(defun serialize-hmm-parameters (hmm s)
  (format s "hmm parameters start~%")

  (format s "lambda-1 ~S~%" (hmm-lambda-1 hmm))
  (format s "lambda-2 ~S~%" (hmm-lambda-2 hmm))
  (format s "lambda-3 ~S~%" (hmm-lambda-3 hmm))
  (format s "theta ~S~%" (hmm-theta hmm))
  (format s "bigram-d ~S~%" (hmm-bigram-d hmm))
  (format s "trigram-d ~S~%" (hmm-trigram-d hmm))

  (format s "hmm parameters end~%"))

(defun serialize-hmm-model (hmm s)
  (serialize-hmm-model-header hmm s)
  (serialize-lexicon (hmm-tag-lexicon hmm) s :hmm-tag-lexicon)
  (serialize-lexicon (hmm-token-lexicon hmm) s :hmm-token-lexicon)
  (serialize-hmm-transitions hmm s)
  (serialize-hmm-emissions hmm s)
  (serialize-hmm-trigram-table hmm s)
  (serialize-hmm-unigram-table hmm s)
  (serialize-hmm-parameters hmm s))

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

(defun deserialize-hmm-transitions (hmm s)
  (let ((transitions (make-array (list (hmm-tag-cardinality hmm) (hmm-tag-cardinality hmm)) :initial-element nil)))
    (unless (equalp (read-line s nil nil)
                    "hmm transitions start")
      (error "HMM model transition table can not be deserialized"))
    (loop for line = (read-line s nil nil)
          until (equalp (string-trim *whitespace* line) "hmm transitions end")

          when (null line)
          do (error "Premature end of file")

          do (destructuring-bind (i j value)
                 (cl-ppcre:all-matches-as-strings "\\S+" (string-trim *whitespace* line))
               (setf (aref transitions
                           (parse-integer i)
                           (parse-integer j))
                     (read-from-string value))))
    transitions))

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

(defun deserialize-hmm-trigram-table (hmm s)
  (let ((trigram-table (make-array (list (hmm-tag-cardinality hmm)
                                         (hmm-tag-cardinality hmm)
                                         (hmm-tag-cardinality hmm))
                                   :initial-element nil)))
    (unless (equalp (read-line s nil nil)
                    "hmm trigram table start")
      (error "HMM model trigram table can not be deserialized"))
    (loop for line = (read-line s nil nil)
          until (equalp (string-trim *whitespace* line) "hmm trigram table end")

          when (null line)
          do (error "Premature end of file")

          do (destructuring-bind (i j k value)
                 (cl-ppcre:all-matches-as-strings "\\S+" (string-trim *whitespace* line))
               (setf (aref trigram-table
                           (parse-integer i)
                           (parse-integer j)
                           (parse-integer k))
                     (read-from-string value))))
    trigram-table))

(defun deserialize-hmm-unigram-table (hmm s)
  (let ((unigram-table (make-array (hmm-tag-cardinality hmm) :initial-element nil)))
    (unless (equalp (read-line s nil nil)
                    "hmm unigram table start")
      (error "HMM model unigram table can not be deserialized"))
    (loop for line = (read-line s nil nil)
          until (equalp (string-trim *whitespace* line) "hmm unigram table end")

          when (null line)
          do (error "Premature end of file")

          do (destructuring-bind (i value)
                 (cl-ppcre:all-matches-as-strings "\\S+" (string-trim *whitespace* line))
               (setf (aref unigram-table
                           (parse-integer i))
                     (read-from-string value))))
    unigram-table))

(defun deserialize-hmm-parameters (s)
  (unless (equalp (read-line s nil nil)
                    "hmm parameters start")
      (error "HMM model unigram table can not be deserialized"))
  (loop for line = (read-line s nil nil)
          until (equalp (string-trim *whitespace* line) "hmm parameters end")

          when (null line)
          do (error "Premature end of file")

          collect (destructuring-bind (param value)
                      (cl-ppcre:all-matches-as-strings "\\S+" (string-trim *whitespace* line))
                    (list (intern (string-upcase param) :keyword)
                          (read-from-string value)))))

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
      (setf (hmm-transitions hmm)
            (deserialize-hmm-transitions hmm s))
      (setf (hmm-emissions hmm)
            (deserialize-hmm-emissions hmm s))
      (setf (hmm-trigram-table hmm)
            (deserialize-hmm-trigram-table hmm s))
      (setf (hmm-unigram-table hmm)
            (deserialize-hmm-unigram-table hmm s))

      (let ((param-alist (deserialize-hmm-parameters s)))
        (loop for param in (mapcar #'first param-alist)
              do (ecase param
                   (:lambda-1 (setf (hmm-lambda-1 hmm)
                                    (second (assoc :lambda-1 param-alist))))
                   (:lambda-2 (setf (hmm-lambda-2 hmm)
                                    (second (assoc :lambda-2 param-alist))))
                   (:lambda-3 (setf (hmm-lambda-3 hmm)
                                    (second (assoc :lambda-3 param-alist))))
                   (:theta (setf (hmm-theta hmm)
                                 (second (assoc :theta param-alist))))
                   (:bigram-d (setf (hmm-bigram-d hmm)
                                    (second (assoc :bigram-d param-alist))))
                   (:trigram-d (setf (hmm-trigram-d hmm)
                                     (second (assoc :trigram-d param-alist)))))))

      (setup-hmm-beam hmm)
      (build-suffix-tries hmm)
    
      hmm)))

(defun deserialize-hmm-model-from-file (file)
  (with-open-file (s file)
    (deserialize-hmm-model s)))
