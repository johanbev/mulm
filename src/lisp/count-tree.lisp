(in-package :mulm)

(defvar *lm-root*)

(locally (declare (optimize (space 3)))
  (defstruct lm-tree-node
    weight
    (adds 0)
    (total 0)
    (emissions (make-lash))
    (children (make-lash))))

(defmethod print-object ((object lm-tree-node) stream)
  (with-slots (total children) object
    (format stream "#<LM-Node ~a decs, ~a total>" (lash-table-count children) total)))

(defun add-sequence (seq lm-root &optional (count 1))
  (let ((children (lm-tree-node-children lm-root)))
    (incf (lm-tree-node-total lm-root) count)
    (let ((child
           (get-or-add-lash (first seq) children (make-lm-tree-node))))
      (if (rest seq)
          (add-sequence (rest seq) child count)
        (incf (lm-tree-node-total child) count)))))

(defun sentence-to-n-grams (sentence n lm-root)
  (declare (type fixnum n))
  (let ((buffer (make-fast-queue :size n :buffer (make-array n))))
    (declare (type fast-queue buffer))
    (declare (dynamic-extent buffer))
    (loop
     ; for i from 0
        for word fixnum in sentence
        for seq = (fast-queue-to-list (fast-enqueue buffer word))
        when (>= (length seq) n)
        do (add-sequence seq lm-root)
        finally (loop 
                    for x on (rest seq)
                    do (add-sequence x lm-root)))))

(defun check-lm (corpus)
  (let* ((hmm (setup-hmm (make-hmm) (corpus-tag-set-size corpus)))
        (n (hmm-n hmm))
         (lm-root (make-lm-tree-node)))
    (populate-counts corpus hmm)
    (loop initially (build-model hmm lm-root)
        for t1 from 0 below n
        for t1-node = (getlash t1 (lm-tree-node-children lm-root))
        when (and (not t1-node) (/= (aref (hmm-unigram-counts hmm) t1) 0))
        do (format t "Inconsistent unigram count ~a - ~a - ~a~%" t1
                   0 (aref (hmm-unigram-counts hmm) t1))
        when t1-node do
          (if (/= (lm-tree-node-total t1-node) (aref (hmm-unigram-counts hmm) t1))
              (format t "Inconsistent unigram count ~a - ~a - ~a~%" t1
                      (lm-tree-node-total t1-node) (aref (hmm-unigram-counts hmm) t1)))
        and do
            (loop
                for t2 from 0 below n
                for t2-node = (getlash t2 (lm-tree-node-children t1-node))
                when (and (not t2-node) (/= (aref (hmm-bigram-counts hmm) t1 t2) 0))
                do (format t "Inconsistent bigram count ~a ~a - ~a - ~a~%" t1 t2
                           0 (aref (hmm-bigram-counts hmm) t1 t2))
                when t2-node do
                  (if (/= (lm-tree-node-total t2-node) (aref (hmm-bigram-counts hmm) t1 t2))
                      (format t "Inconsistent bigram count ~a ~a - ~a - ~a~%" t1 t2
                              (lm-tree-node-total t2-node) (aref (hmm-bigram-counts hmm) t1 t2)))
                and do
                    (loop
                        with total = (float (lm-tree-node-total t2-node))
                        for t3 from 0 below n
                        for t3-node = (getlash t3 (lm-tree-node-children t2-node))
                        when (and (not t3-node) (/= (aref (hmm-trigram-counts hmm) t1 t2 t3) 0))
                        do (format t "Inconsistent bigram count ~a ~a ~a - ~a - ~a~%" t1 t2 t3
                                   0 (aref (hmm-trigram-counts hmm) t1 t2 t3))
                        when t3-node do
                          (if (/= (lm-tree-node-total t3-node) (aref (hmm-trigram-counts hmm) t1 t2 t3))
                              (format t "Inconsistent trigram count ~a ~a ~a - ~a - ~a~%" t1 t2 t3
                                      (lm-tree-node-total t3-node) (aref (hmm-trigram-counts hmm) t1 t2 t3)))
                        and do
                            (let ((prob (/ (lm-tree-node-total t3-node)
                                           total)))
                              (setf (aref (hmm-trigram-table hmm) t1 t2 t3)
                                prob)))))))

(defun build-model (hmm lm-root)
  (let ((c (hmm-tag-cardinality hmm)))
    (loop for i from 0 below c
          for bi-total = 0

          for uni-count = (- (aref (hmm-unigram-counts hmm) i)
                             bi-total)
          
          do (loop for j from 0 below c
                   for tri-total = 0
                   for bi-count = (aref (hmm-bigram-counts hmm) i j)
                   
                   do (loop for k from 0 below c
                            for tri-count = (aref (hmm-trigram-counts hmm) i j k)
                            when (/= tri-count 0)
                            do (add-sequence (list i j k)
                                             lm-root
                                             tri-count)
                            do (incf tri-total tri-count))
                   
                   when (/= (- bi-count tri-total) 0)
                   do (add-sequence (list i j)
                                    lm-root
                                    (- bi-count tri-total))
                   do (incf bi-total bi-count))
          
          when (/= (- uni-count bi-total) 0)
          do (add-sequence (list i) lm-root (- uni-count bi-total)))))
