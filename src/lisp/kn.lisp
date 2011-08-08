(in-package :mulm)

(defun kn-unigrams ()
  (loop
      with unigram-prob = (make-array (hmm-tag-cardinality *hmm*))
      with accu = 0
      for tag from 0 below (hmm-tag-cardinality *hmm*)
      for preceding = (loop
                          for prec from 0 below (hmm-tag-cardinality *hmm*)
                          for node = (getlash prec (lm-tree-node-children *lm-root*))
                          when node
                          summing (if (getlash tag (lm-tree-node-children node))
                                      1
                                    0))
      do (incf accu preceding)
         (setf (aref unigram-prob tag) preceding)
      finally (loop for i below (hmm-tag-cardinality *hmm*)
                  do (setf (aref unigram-prob i)
                       (float (/ (aref unigram-prob i)
                                 accu))))
              (return unigram-prob)))

(defun kn-bigrams (unigrams)
  (loop
      with kn-d = (hmm-bigram-d *hmm*)
      with bigram-probs = (make-array (list (hmm-tag-cardinality *hmm*)
                                            (hmm-tag-cardinality *hmm*))
                                      :element-type 'single-float
                                      :initial-element most-negative-single-float)
      for first from 0 below (hmm-tag-cardinality *hmm*)
      for first-node = (getlash first (lm-tree-node-children *lm-root*))
      for first-decs = (lash-table-count (lm-tree-node-children first-node))
      for first-count = (lm-tree-node-total first-node)
      do (loop
             for second from 0 below (hmm-tag-cardinality *hmm*)
             for second-node = (getlash second (lm-tree-node-children first-node))
             for second-count = (or (and second-node (lm-tree-node-total second-node)) 0)
             for adjusted-count = (max (- second-count kn-d) 0)
             for bigram-prob = (+ (/ adjusted-count first-count) ;; adjusted estimate
                                  (* (/ kn-d first-count)
                                     first-decs
                                     (aref unigrams second)))
             do (setf (aref bigram-probs first second) (if (= 0 bigram-prob)
                                                           most-negative-single-float
                                                         (log bigram-prob))))
      finally (return bigram-probs)))

(defun kn-trigrams (unigrams)
  (loop
      with n = (hmm-tag-cardinality *hmm*)
      with kn-d of-type single-float = (hmm-trigram-d *hmm*)
      with trigram-probs = (make-array
                            (list (hmm-tag-cardinality *hmm*)
                                  (hmm-tag-cardinality *hmm*)
                                  (hmm-tag-cardinality *hmm*))
                            :element-type 'single-float
                            :initial-element most-negative-single-float)
      with top = (lm-tree-node-children *lm-root*)
      for i fixnum below n
      for t1-node = (getlash i top)
      do 
        (loop
            for j fixnum below n            
            for t2-node = (getlash j (lm-tree-node-children t1-node))
            do
              (loop
                  for k fixnum below n
                  ;; first calculate lower order prob:
                  for right-drop = 0.0
                  for right-card = (lash-table-count (lm-tree-node-children (getlash j top)))
                  for left-card =                    
                    (loop
                        for left fixnum below n
                        for left-node = (getlash left top)
                        for t2-node = (getlash j (lm-tree-node-children left-node))
                        for t2-dec fixnum = (or (and t2-node
                                              (lash-table-count (lm-tree-node-children t2-node)))
                                         0)
                        when (and t2-node (getlash k (lm-tree-node-children t2-node)))
                        do (incf right-drop)
                        summing t2-dec)
                    ;;; if left card is 0 then we have either the start tag or end tag
                  when (> left-card 0)
                  do (let* ((alpha-1 (aref unigrams k))
                            (gamma-1 (/ (* right-card (hmm-bigram-d *hmm*)) left-card))
                            (alpha-2 (/ (max 0.0 (- right-drop (hmm-bigram-d *hmm*)))
                                        left-card))
                            (gamma-2 (if t2-node
                                         (/ (* (lash-table-count (lm-tree-node-children t2-node)) kn-d)
                                            (lm-tree-node-total t2-node))
                                       1.0)) ;; I have no idea what this gamma should be really :-(
                            (alpha-3 (if (not t2-node)
                                         0.0
                                       (let ((t3-node (getlash k (lm-tree-node-children t2-node))))
                                         (if t3-node
                                             (/ (max 0.0 (- (lm-tree-node-total t3-node) kn-d))
                                                (lm-tree-node-total t2-node))
                                           0.0))))
                            (prob (+ alpha-3
                                     (* alpha-2 gamma-2)
                                     (* alpha-1 gamma-1))))
                       (declare (type single-float alpha-1 gamma-1 alpha-2 gamma-2 alpha-3 prob))
                       (setf (aref trigram-probs i j k)
                         (if (> prob 0)
                             (log prob)
                           most-negative-single-float)))))
      finally (return trigram-probs)))

(defun estimate-bigram-d (hmm)
  (loop
      with once = 0
      with twice = 0
      for i below (hmm-tag-cardinality hmm)
      do (loop 
             for j below (hmm-tag-cardinality hmm)
             for count = (or (aref (hmm-bigram-counts hmm) i j) -1)
             when (= count 1) do (incf once)
             when (= count 2) do (incf twice))
      finally (return
                (float
                 (if (or (= 0 once) (= 0 twice))
                     1.107
                     (/ once (+ once (* 2 twice))))))))

(defun estimate-trigram-d (hmm)
  (loop
      with once = 0
      with twice = 0
      for i below (hmm-tag-cardinality hmm)
      do (loop 
             for j below (hmm-tag-cardinality hmm)
             do (loop
                    for k below (hmm-tag-cardinality hmm)
                    for count = (or (aref (hmm-trigram-counts hmm) i j k) -1)
                    when (= count 1) do (incf once)
                    when (= count 2) do (incf twice)))
          finally (return
                    (float
                     (if (or (= 0 once) (= 0 twice))
                         1.107
                         (/ once (+ once (* 2 twice))))))))
