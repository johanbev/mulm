(in-package :mulm)

(defparameter *decoder*
    #'viterbi-bigram)

(defun evaluate (hmm corpus)
  (let ((seqs (ll-to-word-list corpus))
        (tag-seqs (ll-to-tag-list corpus)))
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
        for result = (funcall *decoder* hmm seq)
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
        finally (return
                  (values (/ correct total) (/ correct-sequence total-sequence)
                          correct total correct-sequence total-sequence
			  correct-unknown total-unknown unknown-sequence correct-unknown-sequence)))))

(defun do-evaluation (&key (hmm *hmm*) (corpus *wsj-test-corpus*) (clobber t))
  (unless hmm
    (format t "~&do-evaluation(): No model! Using WSJ")
    (unless *wsj-train-corpus*
      (read-wsj-corpus))
    (setf hmm (train *wsj-train-corpus*)))
  (cond
   ((and (eq *decoder* #'viterbi-bigram) clobber)
    (format t "~&do-evaluation(): Ahoy, generating cached transition-table for you")
    (make-transition-table hmm 1 :constant))
   ((and (eq *decoder* #'viterbi-trigram) clobber)
    (format t "~&do-evaluation(): Ahoy, generating cached transition-table for you")
    (make-transition-table hmm 2 :deleted-interpolation))
   (t (format t "~&do-evaluation(): Make sure to have a transition-table")))    
  (format t "~&do-evaluation(): BEGIN evaluation with ~a sequences~%" (length corpus))
  (time
   (multiple-value-bind (acc seqacc correct total cs ts cu tu us cus)
       (evaluate hmm corpus)
     (declare (ignore cs ts))
     (format t "~%Correct:          ~2,4T~a~%" correct)
     (format t "Accuracy:         ~2,4T~,3f %~%" (* acc 100))
     (format t "Sequence Accuracy:~2,4T~,3f %~%" (* seqacc 100))
     (format t "Unknown Token Acc:~2,4T~,3f %~%" (if (> tu 0) (* 100 (/ cu tu)) nil))
     (format t "Unknown Seq Acc:  ~2,4T~,3f %~%" (if (> us 0) (* 100 (/ cus us)) nil))
     (format t "Tokens: ~a, Unknown: ~a, ~,3f %~%" total tu (* 100 (/ tu total))))))
