(in-package :mime)

(defun prepare-corpora (corpora type file)
  (loop
      with reader = (ecase type
                      (:tt #'mulm::read-tt-corpus)
                      (:brown #'mulm::read-brown-file))
      for corpus in corpora
      for path = (make-pathname :directory (pathname-directory file) :name corpus)
      nconcing (funcall reader path)))

(defun split-into-folds (corpus folds)
  "Sequentially split the corpus into folds. ie. each |fold| sentence is held out in turn"
  (loop
      for fold from 1 to folds
      collect (loop
                  with held-out
                  with rest
                  for sequence in corpus
                  for i from fold
                  if (= 0 (mod i folds))
                  do (push sequence held-out)
                  else do (push sequence rest)
                  finally (return (list rest held-out)))))

(defvar *training-curve-steps*
    '(0.01 0.02 0.05 0.10 0.15 0.20 0.30 0.40 0.50 0.7 1))


;; We reuse the fold mechanism here for a quick preview of learning rates
(defun split-into-training-curves (corpus &optional test)
  ;; if test is not supplied split of one tenth for testing
  (let ((working corpus) (held-out test))
    (unless held-out
      (setf working nil)
      (loop
          for sequence in corpus
          for i from 1
          if (= 0 (mod i 10))
          do (push sequence held-out)
          else do (push sequence working)))               
    (loop
        with length = (length corpus)
        for threshold in *training-curve-steps*
        for next-limit = (floor (* threshold length))
        collect
          (list
           (loop
               for seq in working
               for i from 1 below next-limit
               collect seq)
           held-out))))

(defmacro with-experiment (file &body body)
  `(with-open-file (stream ,file :direction :input)
    ;;; this is why we do this in lisp:
    (destructuring-bind
        (experiment
         name
         &key
         corpora
         train
         test
         (print t)
         save
         (suffix-cutoff mulm::*suffix-cutoff*)
         (suffix-freq mulm::*suffix-frequency*)
         (freq-cutoff mulm::*estimation-cutoff*)
         (case-dependent-tries t)
         training-curve
         (gc nil)
         (corpus-type :tt)
         (order 2)
         (tag-split nil)
         (folds 10)
         (smoothing :deleted-interpolation))
        (read stream)
      ,@body)))

(defun make-hmm (train order smoothing)
  (let* ((hmm (mulm::train train)))
    (ecase smoothing
      (:deleted-interpolation
       (setf (mulm::hmm-bigram-transition-table hmm)
         (mulm::make-transition-table hmm 1 :deleted-interpolation))
       (setf (mulm::hmm-trigram-transition-table hmm)
         (mulm::make-transition-table hmm 2 :deleted-interpolation)))
      (:constant 
       (setf (mulm::hmm-bigram-transition-table hmm)
         (mulm::make-transition-table hmm 1 :constant))
       (setf (mulm::hmm-trigram-transition-table hmm)
         (mulm::make-transition-table hmm 2 :constant)))
      (:kn
       (let ((mulm::*lm-root*
              (mulm::hmm-tag-lm hmm))
             (mulm::*hmm* hmm))
         (ecase order
           (1 (setf (mulm::hmm-bigram-transition-table hmm)
                (mulm::kn-bigrams (mulm::kn-unigrams))))
           (2 (setf (mulm::hmm-trigram-transition-table hmm)
                (mulm::kn-trigrams (mulm::kn-bigrams (mulm::kn-unigrams))))
              (setf (mulm::hmm-bigram-transition-table hmm)
                (mulm::kn-bigrams (mulm::kn-unigrams))))))))
    hmm))

(defparameter *working-set* nil)


(defun do-gc (&key full verbose)
  "Initiates a garbage collection."
  (declare (ignorable verbose full))
  #+(or cmu scl) (ext:gc :verbose verbose :full full)
  #+sbcl (sb-ext:gc :full full)
  #+allegro (excl:gc (not (null full)))
  #+clisp (ext:gc)
  #+ecl (si:gc t)
  #+openmcl (ccl:gc)
  #+corman (ccl:gc (if full 3 0))
  #+lispworks (hcl:mark-and-sweep (if full 3 0)))


(defun perform-experiment (file)
  (setf *working-set* nil)
  (format t "reading in experiment file...~%")
  (with-experiment file
    (declare (ignorable experiment))
    (format t "read experiment file...~%")
    (if (and (or train test) corpora)
        (error "Illegal experiment file: Cannot have both train and test set and folds!")
      (progn
        (let (corpus splits
              (mulm::*estimation-cutoff* freq-cutoff)
              (mulm::*suffix-cutoff* suffix-cutoff)
              (mulm::*suffix-frequency* suffix-freq)
              (mulm::*split-tries* case-dependent-tries))
          (cond
           (training-curve
            (setf corpus (prepare-corpora corpora corpus-type file))
            (setf splits (split-into-training-curves corpus)))
           (corpora
            (setf 
                corpus
              (prepare-corpora corpora corpus-type file)
              splits
              (split-into-folds corpus folds)))
            (t (setf
                   splits
                 (list (list (prepare-corpora (list train) corpus-type file)
                             (prepare-corpora (list test) corpus-type file))))))
          (format t "read corpora...~%")          
          (loop
              for (train test) in splits
              for i from 1
              do (format t "~&Doing fold ~a~%" i)
                 (when tag-split
                   (let ((ts (mulm::tag-split-corpora train test)))
                     (setf train (first ts)
                           test (second ts))))
                 (format t "~&Training model....~%")
                 (let ((hmm (make-hmm train order smoothing)))
                   (format t "~&Model trained.")
                   (format t "~&Model has ~a states." (mulm::hmm-n hmm))
                   (format t "~&Decoding ~a sequences" (length test))
                   (let ((decoder (ecase order
                                    (1 #'mulm::viterbi-bigram)
                                    (2 #'mulm::viterbi-trigram))))
                     (loop
                         for forms in (mulm::ll-to-word-list test)
                         for gold-tags in (mulm::ll-to-tag-list test)
                         if forms
                         collect (list 
                                  forms
                                  gold-tags
                                  (funcall decoder hmm forms)) into res
                         else do (format t "~&WARN: Attempt to decode empty sequence, are corpora well formed?~%")
                         finally (push (list (mulm::lexicon-forward (mulm::hmm-token-lexicon hmm))
                                             res train)
                                       *working-set*)
                                 (when gc (setf hmm nil) (do-gc :full t)))))))))
    ;; now register-profile and print it
    (let ((profile (register-profile *working-set* name)))
      (when print
        (typecase print
          (string (with-open-file (stream print :direction :output :if-exists :supersede)
                    (if training-curve
                        (print-lc (learning-curve profile) stream)
                      (print-profile profile stream))))
          (t (if training-curve
                 (print-lc (learning-curve profile))
               (print-profile profile t)))))
      (when save
        (with-open-file (stream save :direction :output :if-exists :supersede)
          (write profile :stream stream))))
    (when gc
      (setf *working-set* nil)
      (do-gc :full t)
      (do-gc))))
