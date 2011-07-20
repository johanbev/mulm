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
         (corpus-type :tt)
         (order 2)
         (folds 10)
         (smoothing :deleted-interpolation))
        (read stream)
      ,@body)))

(defun make-hmm (train order smoothing)
  (let* ((hmm (mulm::train train)))
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

(defun perform-experiment (file)
  (setf *working-set* nil)
  (format t "reading in experiment file...~%")
  (with-experiment file
    (format t "read experiment file...~%")
    (let* ((corpus (prog1 (prepare-corpora corpora corpus-type file) (format t "read corpora...~%")))
           (splits (prog1 (split-into-folds corpus folds) (format t "split into folds...~%"))))
      (loop
          for (train test) in splits
          for i from 1
          do (format t "~&Doing fold ~a~%" i)
             (setf mulm::*known-codes* (make-hash-table))
             (format t "~&Training model....~%")
             (let ((hmm (make-hmm train order smoothing)))
               (format t "~&Model trained.")
               (format t "~&Decoding ~a sequences" (length test))
               (let ((decoder (ecase order
                                    (1 #'mulm::viterbi-bigram)
                                    (2 #'mulm::viterbi-trigram))))
                 (loop
                     for forms in (mulm::ll-to-word-list test)
                     for gold-tags in (mulm::ll-to-tag-list test)
                     collect (list 
                                   forms
                                   gold-tags
                                   (funcall decoder hmm forms)) into res
                     finally (push (list mulm::*known-codes* res train) *working-set*))))))))
                       
                                
               
