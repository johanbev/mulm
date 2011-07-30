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
    (setf mulm::*bigrams* nil)
    (ecase smoothing
      (:deleted-interpolation
       (setf (mulm::hmm-current-transition-table hmm)
         (mulm::make-transition-table hmm order :deleted-interpolation)))
      (:constant (setf (mulm::hmm-current-transition-table hmm)
                   (mulm::make-transition-table hmm order :constant)))
      (:kn
       (let ((mulm::*lm-root*
              (mulm::hmm-tag-lm hmm))
             (mulm::*hmm* hmm))
         (ecase order
           (1 (setf (mulm::hmm-current-transition-table hmm)
                (mulm::kn-bigrams (mulm::kn-unigrams))))
           (2 (setf (mulm::hmm-current-transition-table hmm)
                (mulm::kn-trigrams (mulm::kn-bigrams (mulm::kn-unigrams))))
              (setf mulm::*bigrams*
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
        (let (corpus splits)
          (if corpora
              (setf 
                corpus
                (prepare-corpora corpora corpus-type file)
                splits
                (split-into-folds corpus folds))
            (setf
              splits
              (list (list (prepare-corpora (list train) corpus-type file)
                          (prepare-corpora (list test) corpus-type file)))))
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
                    (print-profile profile stream)))
          (t (print-profile profile print))))
      (when save
        (with-open-file (stream save :direction :output :if-exists :supersede)
          (write profile :stream stream))))
    (when gc
      (setf *working-set* nil)
      (do-gc :full t)
      (do-gc))))
