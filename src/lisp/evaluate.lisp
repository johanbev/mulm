(in-package :mulm)

(defun evaluate-hmm (hmm file &optional (viterbi-func #'viterbi-bigram))
  (with-open-file (stream file :direction :input)
    (loop with total = 0 with correct = 0
          with forms with tags
          for i from 1
          for line = (read-line stream nil)
          for tab = (position #\tab line)
          for form = (normalize-token (subseq line 0 tab))
          for code = (symbol-to-code form)
          for tag = (and tab (subseq line (+ tab 1)))
          while line
          when (and form tag)
          do (push code forms)
          and do (push tag tags)
          else do (loop for gold in (nreverse tags)
                        for tag in (funcall viterbi-func hmm (nreverse forms))
                        do (incf total)
                        when (string= gold tag)
                        do (incf correct))
                        and do (setf forms nil)
                        and do (setf tags nil)
	finally (return (values (/ correct total) correct total)))))



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
	for unknown = (member :unk seq)
	when unknown
	do (incf unknown-sequence)
	when (equal tag-seq result)
	do (incf correct-sequence)
	   (when unknown
	     (incf correct-unknown-sequence))
	do
	  (loop
	      for x in seq
	      for unknown = (eql x :unk)
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

(defun do-evaluation (&key (hmm *hmm*) (corpus *wsj-test-corpus*))
  (format t "~&do-evaluation(): BEGIN evaluation with ~a sequences~%" (length corpus))
  (time
   (multiple-value-bind (acc seqacc correct total cs ts cu tu us cus)
       (evaluate hmm corpus)
     (declare (ignore cs ts))
     (format t "Correct:          ~2,4T~a~%" correct)
     (format t "Accuracy:         ~2,4T~,3f %~%" (* acc 100))
     (format t "Sequence Accuracy:~2,4T~,3f %~%" (* seqacc 100))
     (format t "Unknown Token Acc:~2,4T~,3f %~%" (if (> tu 0) (* 100 (/ cu tu)) nil))
     (format t "Unknown Seq Acc:  ~2,4T~,3f %~%" (if (> us 0) (* 100 (/ cus us)) nil))
     (format t "Tokens: ~a, Unknown: ~a, ~,3f %~%" total tu (* 100 (/ tu total))))))
     
;; Quick function to run standard evaluation
(defun evaluate-all ()
  (let ((hmm (train (read-tt-corpus *wsj-train-file*))))
    (time (multiple-value-bind (accuracy correct total)
              (evaluate-hmm hmm *wsj-eval-file* #'viterbi-bigram)
            (format t "~a / ~a, ~,3f~%" correct total accuracy)))
    (time (multiple-value-bind (accuracy correct total)
              (evaluate-hmm hmm *wsj-eval-file* #'viterbi-trigram)
            (format t "~a / ~a, ~,3f~%" correct total accuracy)))))

;; Bigram results on LW6 pro, Core 2 MacBook Pro (André)
; 11384 / 11890, 0.957
;
; 11384 / 11890, 0.957
;
; User time    =       13.078
; System time  =        0.104
; Elapsed time =       13.019
; Allocation   = 892472224 bytes

;; Deleted interpolation trigram results on LW6 pro, Core 2 MacBook Pro (André)
; 11303 / 11890, 0.951
;
; User time    =  0:02:12.856
; System time  =        0.656
; Elapsed time =  0:02:13.070
; Allocation   = 7563386340 bytestes
; 
;; Results on Allegro 8.1, Core 2 2.4 GHz:
; cpu time (non-gc) 4,610 msec user, 90 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  4,610 msec user, 90 msec system
; real time  4,795 msec
; space allocation:
;  39,947 cons cells, 230,461,728 other bytes, 0 static bytes
;; 11383/11890

;; Results on SBCL 1.0.48 Core 2 2.4 GHz:

;; Evaluation took:
;;   5.371 seconds of real time
;;   5.372983 seconds of total run time (5.286322 user, 0.086661 system)
;;   [ Run times consist of 0.189 seconds GC time, and 5.184 seconds non-GC time. ]
;;   100.04% CPU
;;   12,917,586,087 processor cycles
;;   221,779,600 bytes consed
  
;; 11383/11890
