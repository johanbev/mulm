(in-package :mulm)

(defun evaluate (decoder corpus &key (seq-handler nil))
  (let ((seqs (ll-to-word-list corpus))
        (tag-seqs (ll-to-tag-list corpus))
        (hmm (decoder-model decoder))
        ; (constraints (ll-to-constraint-list corpus))
        )
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
        ; for constraint in constraints
        for result = (and seq (decode decoder seq))
        for unknown = (loop 
                          for x in seq 
                          when (not (token-to-code x (hmm-token-lexicon hmm)))
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
              for unknown = (not (token-to-code x (hmm-token-lexicon hmm)))
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

(defun do-evaluation (&key (decoder nil)
                           (corpus *wsj-test-corpus*)
                           (order 1)
                           (smoothing :deleted-interpolation)
                           (marker nil))
  ;; make sure default corpora are initialized if needed
  (unless (and corpus decoder)
    (read-wsj-corpus))
  (unless corpus
    (setf corpus *wsj-test-corpus*))
  (unless decoder
    (log5:log-for (log5:info) "Generating decoder from corpus")
    (setf decoder (make-decoder-from-corpus *wsj-train-corpus*
                                            (make-description :order order
                                                              :smoothing smoothing))))    
  (log5:log-for (log5:info) "Evaluating with ~a sequences~%" (length corpus))
  (let ((seq-handler (if marker
                       #'(lambda (total)
                           (when (and marker
                                      (= (rem total marker) 0))
                             (write-char #\.)
                             (finish-output))))))
    (multiple-value-bind (acc seqacc correct total cs ts cu tu us cus)
        (evaluate decoder corpus
                  :seq-handler seq-handler)
      (declare (ignore cs ts))
      (format t "~%Order:            ~a~%" order)
      (format t "Correct:          ~2,4T~a~%" correct)
      (format t "Accuracy:         ~2,4T~,3f %~%" (* acc 100))
      (format t "Sequence Accuracy:~2,4T~,3f %~%" (* seqacc 100))
      (format t "Unknown Token Acc:~2,4T~,3f %~%" (if (> tu 0) (* 100 (/ cu tu)) nil))
      (format t "Unknown Seq Acc:  ~2,4T~,3f %~%" (if (> us 0) (* 100 (/ cus us)) nil))
      (format t "Tokens: ~a, Unknown: ~a, ~,3f %~%" total tu (* 100 (/ tu total)))

      (values (float acc) correct total))))
