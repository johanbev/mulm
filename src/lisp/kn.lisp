(in-package :mulm)

(defun kn-unigrams ()
  (loop
      with unigram-prob = (make-array (hmm-n *hmm*))
      with accu = 0
      for tag from 0 below (hmm-n *hmm*)
      for preceding = (loop
                          for prec from 0 below (hmm-n *hmm*)
                          for node = (gethash prec (lm-tree-node-children *lm-root*))
                          when node
                          summing (if (gethash tag (lm-tree-node-children node))
                                      1
                                    0))
      do (incf accu preceding)
         (setf (aref unigram-prob tag) preceding)
      finally (loop for i below (hmm-n *hmm*)
                  do (setf (aref unigram-prob i)
                       (float (/ (aref unigram-prob i)
                                 accu))))
              (return unigram-prob)))

(defun kn-bigrams (unigrams)
  (loop
      with kn-d = (hmm-bigram-d *hmm*)
      with bigram-probs = (make-array (list (hmm-n *hmm*)
                                            (hmm-n *hmm*))
                                      :element-type 'single-float
                                      :initial-element most-negative-single-float)
      for first from 0 below (hmm-n *hmm*)
      for first-node = (gethash first (lm-tree-node-children *lm-root*))
      for first-decs = (hash-table-count (lm-tree-node-children first-node))
      for first-count = (lm-tree-node-total first-node)
      do (loop
             for second from 0 below (hmm-n *hmm*)
             for second-node = (gethash second (lm-tree-node-children first-node))
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

(defun kn-trigrams (bigrams)
  (loop
      with kn-d of-type single-float = (hmm-trigram-d *hmm*)
      with trigram-probs = (make-array
                            (list (hmm-n *hmm*)
                                  (hmm-n *hmm*)
                                  (hmm-n *hmm*))
                            :element-type 'single-float
                            :initial-element most-negative-single-float)
      for first fixnum from 0 below (hmm-n *hmm*)                             
      for first-node = (gethash first (lm-tree-node-children *lm-root*))
      do (loop
             for second fixnum from 0 below (hmm-n *hmm*)
             for second-node = (gethash second (lm-tree-node-children first-node))
             for second-count fixnum = (or (and second-node (lm-tree-node-total second-node)) 0)
             for second-decs = (and second-node (hash-table-count (lm-tree-node-children second-node)))
             when second-node do
               (loop
                   for third fixnum from 0 below (hmm-n *hmm*)
                   for third-node = (gethash third (lm-tree-node-children second-node))
                   for third-count fixnum  = (or (and third-node (lm-tree-node-total third-node)) 0)
                   for adjusted-count of-type single-float = (max (- third-count kn-d) 0.0)
                   for bigram of-type single-float = (aref (the (simple-array single-float (* *)) bigrams) second third)
                   for trigram-prob = (+ (the single-float (/ adjusted-count (the single-float (float second-count))))
                                         (* (the single-float (/ (the single-float kn-d)
                                                                 (the single-float (float second-count))))
                                            (the single-float (float second-decs))
                                            (the single-float  (if (< bigram -100.0)
                                                                   0.0
                                                                 (exp bigram)))))
                   do (setf (aref trigram-probs first second third)
                        (if (= 0 trigram-prob)
                            most-negative-single-float
                          (float (log trigram-prob))))))
      finally (return trigram-probs)))



(defun estimate-bigram-d (hmm)
  (loop
      with once = 0
      with twice = 0
      for i below (hmm-n hmm)
      do (loop 
             for j below (hmm-n hmm)
             for count = (or (aref (hmm-transitions hmm) i j) -1)
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
      for i below (hmm-n hmm)
      do (loop 
             for j below (hmm-n hmm)
             do (loop
                    for k below (hmm-n hmm)
                    for count = (or (aref (hmm-trigram-table hmm) i j k) -1)
                    when (= count 1) do (incf once)
                    when (= count 2) do (incf twice)))
          finally (return
                    (float
                     (if (or (= 0 once) (= 0 twice))
                         1.107
                         (/ once (+ once (* 2 twice))))))))
