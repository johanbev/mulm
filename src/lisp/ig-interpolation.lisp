(in-package :mulm)

(defparameter *ent-cache* (make-hash-table :test #'equal))
(defparameter *uni-cache* (make-hash-table))
(defparameter *bi-cache* (make-hash-table :test #'equal))

(defun clear-cache ()
  (clrhash *ent-cache*)
  (clrhash *uni-cache*)
  (clrhash *bi-cache*))
  
(defun memoized-lash-entropy (lash-table &key (key #'identity))
  (get-or-add lash-table *ent-cache* (lash-table-entropy lash-table :key key)))

(defun memoized-uni-gain (hmm t1 unigram-entropy)
  (get-or-add t1 *uni-cache* (direct-unigram-information-gain hmm t1 unigram-entropy)))

(defun memoized-bi-gain (hmm t1 t2 unigram-entropy)
  (get-or-add (cons t1 t2) *bi-cache* (bigram-information-gain hmm t1 t2 unigram-entropy)))
    

(defun unigram-entropy (hmm)
  (loop
      for prob across (hmm-unigram-probs hmm)
      summing (* -1.0 prob (log prob 2.0))))

(defun bigram-information-gain (hmm t1 t2 unigram-entropy)
  (if  (aref (hmm-transitions hmm) t1 t2)
      (loop
          with root = (hmm-tag-lm hmm)
          with first-node = (getlash t1 (lm-tree-node-children root))
          with second-node = (getlash t2 (lm-tree-node-children first-node))
          with hx = unigram-entropy
          with pe = (/ (aref (hmm-bigram-counts hmm) t1 t2)  (hmm-token-count hmm))  ;; not correct.
          with p-not-e = (- 1.0 pe)
          with he = (memoized-lash-entropy (lm-tree-node-children second-node) :key #'lm-tree-node-total)
          with positive-gain = (* -1.0 pe he)
          ;;; now calc negative gain
          with reduced-total = (float (- (reduce #'+ (hmm-unigram-counts hmm)) (lm-tree-node-total second-node)))
          for tag below (hmm-tag-cardinality hmm)
          for reduced-count = (- (aref (hmm-unigram-counts hmm) tag)
                                 (if (getlash tag (lm-tree-node-children second-node))
                                     (lm-tree-node-total (getlash tag (lm-tree-node-children second-node)))
                                   0))
          for reduced-prob = (if (= 0.0 reduced-total)
                                 0.0
                               (/ reduced-count reduced-total))
          summing (if (>= 0.0 reduced-prob)
                      0.0
                    (* reduced-prob (log reduced-prob 2.0))) into accu
          finally 
            (let ((ig (+ hx
                        positive-gain
                        (* p-not-e accu))))
              (if (> ig 0.0)
                  (return ig)
                (return 0.0))))
    0.0))

(defun direct-unigram-information-gain (hmm t1 unigram-entropy)
  (if (aref (hmm-unigram-probs hmm) t1)
      (loop
          with root = (hmm-tag-lm hmm)
          with first-node = (getlash t1 (lm-tree-node-children root))
          with hx = unigram-entropy
          with pe = (aref (hmm-unigram-probs hmm) t1)
          with p-not-e = (- 1.0 pe)
          with he = (memoized-lash-entropy (lm-tree-node-children first-node) :key #'lm-tree-node-total)
          with positive-gain = (* -1.0 pe he)
          with reduced-total = (float (- (reduce #'+ (hmm-unigram-counts hmm)) (lm-tree-node-total first-node)))
          for tag below (hmm-tag-cardinality hmm)
          for reduced-count = (- (aref (hmm-unigram-counts hmm) tag)
                                 (if (getlash tag (lm-tree-node-children first-node))
                                     (lm-tree-node-total (getlash tag (lm-tree-node-children first-node)))
                                   0))
          for reduced-prob = (if (= 0.0 reduced-total)
                                 0.0
                               (/ reduced-count reduced-total))
          summing (if (>= 0.0 reduced-prob)
                      0.0
                    (* reduced-prob (log reduced-prob 2.0))) into accu
          finally
            (return (values
                     (+ hx
                        positive-gain
                        (* p-not-e accu))
                     hx
                     positive-gain
                     (* p-not-e accu))))
    0.0))


(defun ig-interpolated-trigram-prob (hmm t1 t2 t3 entropy)
  (let* ((lambda2  (memoized-uni-gain hmm t2 entropy))
         (lambda3  (memoized-bi-gain hmm t1 t2 entropy))
         (lambda1 (/ 1.0 (hmm-tag-cardinality hmm)))
         (ig-lambda-z (+ lambda2 lambda3 lambda1))
         (scaling-z 1)
         (unigram (or (aref (hmm-unigram-probs hmm) t3) 0.0))
         (bigram (or (aref (hmm-transitions hmm) t2 t3) 0.0))
         (trigram (or (aref (hmm-trigram-probs hmm) t1 t2 t3) 0.0)))
    (let ((prob (log (+ (* (/ lambda1 ig-lambda-z) unigram)
                        (* scaling-z (/ lambda2 ig-lambda-z) bigram)
                        (* scaling-z (/ lambda3 ig-lambda-z) trigram)))))
      (if (complexp prob)
          (error (format nil "ACH: ~a ~a ~a ~a ~a ~a ~a~%" lambda1 lambda2 lambda3 ig-lambda-z unigram bigram trigram))
        prob))))

(defun ig-interpolated-bigram-prob (hmm t1 t2 entropy)
  (let* ((lambda2 (memoized-uni-gain hmm t2 entropy))
         (lambda1 (/ 1.0 (hmm-tag-cardinality hmm)))
         (ig-lambda-z (+ lambda2 lambda1))
         (unigram (or (aref (hmm-unigram-probs hmm) t2) 0.0))
         (bigram (or (aref (hmm-transitions hmm) t1 t2) 0.0)))
    (values (log (+ (* (/ lambda2 ig-lambda-z) bigram)
                    (* (/ lambda1 ig-lambda-z) unigram))))))
                   

(defun make-ig-transition-table (hmm)
  (clear-cache)
  (let ((tri-table (make-array (list (hmm-tag-cardinality hmm)
                                     (hmm-tag-cardinality hmm)
                                     (hmm-tag-cardinality hmm))
                               :element-type 'single-float
                               :initial-element most-negative-single-float))
        (bi-table (make-array (list (hmm-tag-cardinality hmm)
                                    (hmm-tag-cardinality hmm))
                              :element-type 'single-float
                              :initial-element most-negative-single-float)))
    (loop
     with tag-card = (hmm-tag-cardinality hmm)
     with entropy = (unigram-entropy hmm)
     for i below tag-card do
     (loop for j below tag-card
           do (setf (aref bi-table i j)
                    (ig-interpolated-bigram-prob hmm i j entropy))
           (loop for k below tag-card do
                 (setf (aref tri-table i j k)
                       (ig-interpolated-trigram-prob hmm i j k entropy)))))
    (list bi-table tri-table)))
