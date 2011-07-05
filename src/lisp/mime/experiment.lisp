(in-package :mime)

(defun prepare-corpora (corpora type file)
  (loop
      with reader = (ecase type
                      (:tt #'mulm::read-tt-corpus)
                      (:brown #'mulm::read-brown-corpus))
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
                  for i from 0
                  if (= 0 (rem i (+ fold folds)))
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
      (:deleted-interpolation )
      (:constant (setf (mulm::hmm-current-transition-table hmm)
                   (mulm::make-transition-table hmm order :constant)))
      (:tt
       (mulm::create-kn-count-tree train order hmm)
       (ecase order
         (1 (setf (mulm::hmm-current-transition-table hmm)
              (mulm::kn-bigrams (mulm::kn-unigrams))))
         (2 (setf (mulm::hmm-current-transition-table hmm)
              (mulm::kn-trigrams (mulm::kn-bigrams (mulm::kn-unigrams))))
            (setf mulm::*bigrams*
              (mulm::kn-bigrams (mulm::kn-unigrams)))))))
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
          do (format t "Doing fold ~a~%" i)
             (setf mulm::*known-codes* (make-hash-table))
             (format t "Training model....~%")
             (let ((hmm (make-hmm train order smoothing)))
               (mulm::do-evaluation :order order :corpus test :hmm hmm :clobber clobber))))))
             
      
      
           
