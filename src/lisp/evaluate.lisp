(in-package :mulm)

(defun evaluate-hmm (hmm file)
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
                        for tag in (viterbi hmm (nreverse forms))
                        do (incf total)
                        when (string= gold tag)
                        do (incf correct))
                        and do (setf forms nil)
                        and do (setf tags nil)
                        finally (return (values (/ correct total) correct total)))))

;; Quick function to run standard evaluation
(defun evaluate-all ()
  (time (multiple-value-bind (accuracy correct total)
            (evaluate-hmm (train (read-tt-corpus *tagger-train-file*))
                      *tagger-eval-file*)
          (format t "~a / ~a, ~,3f~%" correct total accuracy))))

;; Results on LW6 pro, Core 2 MacBook Pro (André)
; 11388 / 11890, 0.958
; 
; User time    =       15.823
; System time  =        0.139
; Elapsed time =       16.045
; Allocation   = 500389196 bytes
; 16378 Page faults

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
