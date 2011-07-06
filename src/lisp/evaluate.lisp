(in-package :mulm)

(defun evaluate (hmm corpus decoder-func &key (seq-handler nil) (decoder nil))
  (let ((seqs (ll-to-word-list corpus))
        (tag-seqs (ll-to-tag-list corpus))
        (constraints (ll-to-constraint-list corpus)))
    (loop
        with correct = 0
        with total = 0
        with correct-unknown = 0
        with total-unknown = 0
        with correct-sequence = 0
        with unknown-sequence = 0
        with correct-unknown-sequence = 0
        for total-sequence from 0
        for seq in seqs
        for tag-seq in tag-seqs
        for constraint in constraints
        for result = (and seq (funcall decoder-func hmm seq :decoder decoder :constraints constraint))
        for unknown = (loop 
                          for x in seq 
                          when (not (gethash x *known-codes*))
                          do (return t) 
                          finally (return nil))
        when unknown
        do (incf unknown-sequence)
        when (equal tag-seq result)
        do (incf correct-sequence)
           (when unknown
             (incf correct-unknown-sequence))
        do
          (loop
              for x in seq
              for unknown = (not (gethash x *known-codes*))
              for blue in result
              for gold in tag-seq
              when unknown do (incf total-unknown)
              when (equal blue gold)
              do (incf correct) (when unknown (incf correct-unknown))
              do (incf total))
        when seq-handler
        do (funcall seq-handler total)
        finally (return
                  (values (/ correct total) (/ correct-sequence total-sequence)
                          correct total correct-sequence total-sequence
			  correct-unknown total-unknown unknown-sequence correct-unknown-sequence)))))

(defun do-evaluation (&key (order 1)
                           (decoder nil)
                           (hmm *hmm*)
                           (corpus *wsj-test-corpus*)
                           (clobber t)
                           (marker nil))
  (when decoder
    (setf hmm (viterbi-decoder-model decoder)))
  (unless hmm
    (format t "~&do-evaluation(): No model! Using WSJ")
    (unless *wsj-train-corpus*
      (read-wsj-corpus))
    (setf hmm (train *wsj-train-corpus*)))
  (unless corpus
    (setf corpus (read-tt-corpus *wsj-eval-file*)))
  (cond
   ((and (= order 1) clobber)
    (format t "~&do-evaluation(): Ahoy, generating cached transition-table for you")
    (make-transition-table hmm 1 :constant))
   ((and (= order 2) clobber)
    (format t "~&do-evaluation(): Ahoy, generating cached transition-table for you")
    (make-transition-table hmm 2 :deleted-interpolation))
   (t (format t "~&do-evaluation(): Make sure to have a transition-table")))    
  (format t "~&do-evaluation(): BEGIN evaluation with ~a sequences~%" (length corpus))
  (let ((func (or (and decoder (viterbi-decoder-function decoder))
                  (ecase order
                    (1 #'viterbi-bigram)
                    (2 #'viterbi-trigram))))
        (seq-handler (if marker
                       #'(lambda (total)
                                    (when (and marker
                                               (= (rem total marker) 0))
                                      (write-char #\.))))))
    (multiple-value-bind (acc seqacc correct total cs ts cu tu us cus)
         (evaluate hmm corpus func
                   :seq-handler seq-handler
                   :decoder decoder)
       (declare (ignore cs ts))
       (format t "~%Order:            ~a~%" (if decoder func order))
       (format t "Correct:          ~2,4T~a~%" correct)
       (format t "Accuracy:         ~2,4T~,3f %~%" (* acc 100))
       (format t "Sequence Accuracy:~2,4T~,3f %~%" (* seqacc 100))
       (format t "Unknown Token Acc:~2,4T~,3f %~%" (if (> tu 0) (* 100 (/ cu tu)) nil))
       (format t "Unknown Seq Acc:  ~2,4T~,3f %~%" (if (> us 0) (* 100 (/ cus us)) nil))
       (format t "Tokens: ~a, Unknown: ~a, ~,3f %~%" total tu (* 100 (/ tu total))))))
