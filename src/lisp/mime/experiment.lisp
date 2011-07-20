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
         (tag-split nil)
         (folds 10)
         (smoothing :deleted-interpolation))
        (read stream)
      ,@body)))

(defun make-hmm (train order smoothing)
  (let* ((hmm (mulm::train train)))
    (ecase smoothing
      (:deleted-interpolation )
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
  

(defun perform-experiment (file)
  (format t "reading in experiment file...~%")
  (with-experiment file
    (format t "read experiment file...~%")
    (let* ((corpus (prog1 (prepare-corpora corpora corpus-type file) (format t "read corpora...~%")))
           (splits (prog1 (split-into-folds corpus folds) (format t "split into folds...~%")))
           (clobber (eql smoothing :deleted-interpolation)))
      (loop
          for (train test) in splits
          for i from 1
          do (format t "~&Doing fold ~a~%" i)
             (when tag-split
               (let ((ts (mulm::tag-split-corpora train test)))
                 (setf train (first ts)
                       test (second ts))))
             (setf mulm::*known-codes* (make-hash-table))
             (format t "~&Training model....~%")
             (let ((hmm (make-hmm train order smoothing)))
               (mulm::do-evaluation :order order :corpus test :hmm hmm :clobber clobber))))))
