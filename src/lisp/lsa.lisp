(in-package :mulm)

(defparameter *ent-cache* (make-hash-table :test #'equal))

(defun clear-cache ()
  (clrhash *ent-cache*))
  
(defun memoized-lash-entropy (lash-table &key (key #'identity))
  (get-or-add lash-table *ent-cache* (lash-table-entropy lash-table :key key)))

(defun unigram-entropy (hmm)
  (loop
      for prob across (hmm-unigram-table hmm)
      summing (* -1.0 prob (log prob 2.0))))


;;; Normalized for all t1 except end-tag.
(defun bigram-lsa (hmm t1 t2 unigram-entropy)
  ;; P-lsa (t2 | t1) =  (L(t1) * P-emp(t2 | t1) + P-lsa(t1)) / (L(t1) + 1)
  ;; L(t1) = sqrt(12) * sqrt(C(t1))*e^-H(t1)
  ;; TnT set P-lsa(t1) approx P-emp(t1)
  (let* ((lt1 (* (sqrt 12)
                 (sqrt (hmm-token-count hmm))
                 (exp (* -1.0 unigram-entropy)))))
    (/ (+ (* (or (aref (hmm-transitions hmm) t1 t2) 0.0)
             lt1)
          (aref (hmm-unigram-table hmm) t2))
       (+ lt1 1))))

(defun trigram-lsa (hmm t1 t2 t3 t3t2-lsa)
  ;; P-sa (t3 | t1t2) = (L(t1t2) * P-emp (t3 | t1 t2) + P-lsa (t3 | t2)) / (L(t1t2) + 1)
  (let* ((node (getlash t2 (lm-tree-node-children (getlash t1 (lm-tree-node-children (hmm-tag-lm hmm))))))
         (lt2 (if (not node)
                  0.0
                (* (sqrt 12)
                   (sqrt (lm-tree-node-total hmm))
                   (exp (* -1.0 (memoized-lash-entropy (lm-tree-node-children node) :key #'lm-tree-node-total)))))))
    (/ (+ (* (or (aref (hmm-trigram-table hmm) t1 t2 t3) 0.0)
             lt2)
          t3t2-lsa)
       (+ lt2 1))))

(defun make-lsa-transition-table (hmm)
  (clear-cache)
   (unless (hmm-trigram-transition-table hmm)
    (setf (hmm-trigram-transition-table hmm)
      (make-array (list (hmm-tag-cardinality hmm)
                        (hmm-tag-cardinality hmm)
                        (hmm-tag-cardinality hmm))
                  :element-type 'single-float
                  :initial-element most-negative-single-float)))
  (unless (hmm-bigram-transition-table hmm)
    (setf (hmm-bigram-transition-table hmm)
      (make-array (list (hmm-tag-cardinality hmm)
                        (hmm-tag-cardinality hmm))
                  :element-type 'single-float
                  :initial-element most-negative-single-float)))
  (loop
      with tag-card = (hmm-tag-cardinality hmm)
      with unigram-entropy = (unigram-entropy hmm)
      for i below tag-card do
        (loop 
            for j below tag-card
            for bigram-prob = (bigram-lsa hmm i j unigram-entropy)
            do (setf (aref (hmm-bigram-transition-table hmm) i j)
                 (log bigram-prob))
               (loop for k below tag-card do
                     (setf (aref (hmm-trigram-transition-table hmm) i j k)
                       (log (trigram-lsa hmm i j k (bigram-lsa hmm j k unigram-entropy))))))))
