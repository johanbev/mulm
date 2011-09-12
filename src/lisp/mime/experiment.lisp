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
  (log5:log-for (log5:info) "Reading experiment file")
  (with-experiment file
    (declare (ignorable experiment))
    (log5:log-for (log5:info) "Finished reading experiment file")
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
          (log5:log-for (log5:info) "Finished reading corpora")          
          (loop
              for (train test) in splits
              for i from 1
              do (log5:log-for (log5:info) "Doing fold ~a" i)
                 (when tag-split
                   (let ((ts (mulm::tag-split-corpora train test)))
                     (setf train (first ts)
                           test (second ts))))
                 (log5:log-for (log5:info) "Training model")
                 (let* ((decoder (mulm::make-decoder-from-corpus train
                                                                 (mulm::make-description :order order
                                                                                         :smoothing smoothing)))
                        (hmm (mulm::decoder-model decoder)))
                   (log5:log-for (log5:info) "Model trained")
                   (log5:log-for (log5:info) "Model has ~a states" (mulm::hmm-n hmm))
                   (log5:log-for (log5:info) "Decoding ~a sequences" (length test))
                   (loop
                         for forms in (mulm::ll-to-word-list test)
                         for gold-tags in (mulm::ll-to-tag-list test)
                         if forms
                         collect (list 
                                  forms
                                  gold-tags
                                  (mulm::decode decoder forms)) into res
                         else do (log5:log-for (log5:warn) "Attempt to decode empty sequence, are corpora well formed?")
                         finally (push (list (mulm::lexicon-forward (mulm::hmm-token-lexicon hmm))
                                             res train)
                                       *working-set*)
                                 (when gc (setf hmm nil) (do-gc :full t))))))))
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
          (write profile :stream stream)))
      (when gc
        (setf *working-set* nil)
        (do-gc :full t)
        (do-gc))

      (loop for fold in (profile-folds profile)
            do (setf (fold-train fold) nil)
            do (setf (fold-results fold) nil))
      
      profile)))
