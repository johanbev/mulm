(in-package :mime)

(defstruct experiment
  (name)
  (path)
  (system :mulm)
  (corpora)
  (train)
  (test)
  (print t)
  (save)
  (suffix-cutoff mulm::*suffix-cutoff*)
  (suffix-freq mulm::*suffix-frequency*)
  (freq-cutoff mulm::*estimation-cutoff*)
  (case-dependent-tries t)
  (training-curve)
  (gc nil)
  (corpus-type :tt)
  (order 2)
  (tag-split nil)
  (folds 10)
  (smoothing :deleted-interpolation))

(defstruct system
  (build-model-handler)
  (predict-handler)
  (scoring-handler))

(defun prepare-corpora (corpora type path)
  (loop
      with reader = (ecase type
                      (:tt #'mulm::read-tt-corpus)
                      (:brown #'mulm::read-brown-file))
      for corpus in corpora
      for full-path = (make-pathname :directory path :name corpus)
      nconcing (funcall reader full-path)))

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


(defun make-model-handler (e train)
  (mulm::make-decoder-from-corpus
   train
   (mulm::make-description :order (experiment-order e)
                           :smoothing (experiment-smoothing e))))

(defun predict-handler (decoder forms)
  (mulm::decode decoder forms))

(defun init-system (system-type)
  (cond ((eql system-type :mulm)
         (make-system :build-model-handler #'make-model-handler
                      :predict-handler #'predict-handler))))

(defun read-experiment-file (file)
  (with-open-file (stream file :direction :input)
    ;;; this is why we do this in lisp:
    (destructuring-bind
        (experiment
         name
         &key
         (system :mulm)
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
      (declare (ignore experiment))
      (make-experiment :name name
                       :path (pathname-directory file)
                       :system (init-system system)
                       :corpora corpora
                       :train train
                       :test test
                       :print print
                       :save save
                       :suffix-cutoff suffix-cutoff
                       :suffix-freq suffix-freq
                       :freq-cutoff freq-cutoff
                       :case-dependent-tries case-dependent-tries
                       :training-curve training-curve
                       :gc gc
                       :corpus-type corpus-type
                       :order order
                       :tag-split tag-split
                       :folds folds
                       :smoothing smoothing))))

(defmacro with-experiment-file ((experiment file) &body body)
  `(let* ((,experiment (read-experiment-file ,file)))
     ,@body))

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

(defun perform-experiment-with-file (file)
  (log5:log-for (log5:info) "Finished reading experiment file ~a" file)
  (with-experiment-file (e file)
    (when (and (or (experiment-train e) (experiment-test e))
               (experiment-corpora e))
      (error "Illegal experiment file: Cannot have both train and test set and folds!"))
    (perform-experiment e)))

(defun perform-experiment (e)
  (setf *working-set* nil)
  (progn
    (let (corpus
          splits
          (mulm::*estimation-cutoff* (experiment-freq-cutoff e))
          (mulm::*suffix-cutoff* (experiment-suffix-cutoff e))
          (mulm::*suffix-frequency* (experiment-suffix-freq e))
          (mulm::*split-tries* (experiment-case-dependent-tries e)))
      (cond
       ((experiment-training-curve e)
        (setf corpus (prepare-corpora (experiment-corpora e)
                                      (experiment-corpus-type e)
                                      (experiment-path e)))
        (setf splits (split-into-training-curves corpus)))
       ((experiment-corpora e)
        (setf corpus
              (prepare-corpora (experiment-corpora e)
                               (experiment-corpus-type e)
                               (experiment-path e))
              splits
              (split-into-folds corpus (experiment-folds e))))
       (t (setf splits
                (list (list (prepare-corpora (list (experiment-train e))
                                             (experiment-corpus-type e)
                                             (experiment-path e))
                            (prepare-corpora (list (experiment-test e))
                                             (experiment-corpus-type e)
                                             (experiment-path e)))))))
      (log5:log-for (log5:info) "Finished reading corpora")          
      (loop
       for (train test) in splits
       for i from 1
       do (log5:log-for (log5:info) "Doing fold ~a" i)
       (when (experiment-tag-split e)
         (log5:log-for (log5:info) "Tag splitting corpus" i)
         (let ((ts (mulm::tag-split-corpora train test)))
           (setf train (first ts)
                 test (second ts))))
       
       (log5:log-for (log5:info) "Training model")
       (let* ((decoder (make-model-handler e train))
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
                   (predict-handler decoder forms)) into res
          else do (log5:log-for (log5:warn)
                                "Attempt to decode empty sequence, are corpora well formed?")
          finally (push (list (mulm::lexicon-forward (mulm::hmm-token-lexicon hmm))
                              res train)
                        *working-set*)
          (when (experiment-gc e)
            (setf hmm nil)
            (do-gc :full t)))))))
  ;; now register-profile and print it
  (let ((profile (register-profile *working-set* (experiment-name e))))
    (when (experiment-print e)
      (typecase (experiment-print e)
        (string (with-open-file (stream (experiment-print e)
                                        :direction :output :if-exists :supersede)
                  (if (experiment-training-curve e)
                    (print-lc (learning-curve profile) stream)
                    (print-profile profile stream))))
        (t (if (experiment-training-curve e)
             (print-lc (learning-curve profile))
             (print-profile profile t)))))
    (when (experiment-save e)
      (with-open-file (stream (experiment-save e) :direction :output :if-exists :supersede)
        (write profile :stream stream)))
    (when (experiment-gc e)
      (setf *working-set* nil)
      (do-gc :full t)
      (do-gc))

    (loop for fold in (profile-folds profile)
          do (setf (fold-train fold) nil)
          do (setf (fold-results fold) nil))
      
    profile))
