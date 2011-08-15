(in-package :mulm)

(defvar *suffix-trie-root*)

(defparameter *suffix-cutoff* 10)

(defun add-word (hmm word tag count node)
  (let* ((form  (code-to-token word (hmm-token-lexicon hmm)))
         (nodes (coerce (if (> (length form) *suffix-cutoff*)
                            (subseq form (- (length form)  *suffix-cutoff*))
                          form)
                        'list)))
    (incf (lm-tree-node-adds node) count)
    (incf (getlash tag (lm-tree-node-emissions node) 0) count)
    (loop 
        for seq on nodes
        do (add-word-to-trie seq tag (abs count) node))))

;;; Add the word (character sequence) to the trie. When we reach the end of the
;;; sequence we add the emission
(defun add-word-to-trie (word tag count &optional (node *suffix-trie-root*))
  (declare (type lm-tree-node node)
           ; (fixnum count tag)
           )
  (let ((children (lm-tree-node-children node)))
    (incf (lm-tree-node-total node)  count)
    (let* ((rest (rest word))
           (first (first word))
           (child-node (get-or-add-lash first children (make-lm-tree-node))))
      (if rest
          (add-word-to-trie rest tag count child-node)
        (progn
          (incf (lm-tree-node-total child-node) count)
          (incf (getlash tag (lm-tree-node-emissions child-node) 0) count))))))

(defun weight-and-dist-of (word node)
  "Walk the suffix trie and return the weight of the end node"
  (let* ((rest (rest word))
         (first (first word))
         (child (getlash first (lm-tree-node-children node))))
    (if (null child)
        nil
      (progn
        (if (null rest)
            (list (lm-tree-node-weight child)
                  child)
          (weight-and-dist-of rest child))))))


(defparameter *default-suffix-weighting*
    'diff-entropy)

(defun compute-suffix-trie-weights (trie)
  "Compute the weights in a trie"
  ;;
  (case *default-suffix-weighting*
    (diff-entropy
     (let* ((root-entropy (lash-table-entropy (lm-tree-node-emissions trie)))
            (fcn (compile nil 
                         (lambda (node)
                           (diff-entropy-suffix-weighting node root-entropy)))))
       (compute-suffix-weights trie fcn)))
    (ig
     (let* ((root-entropy (lash-table-entropy (lm-tree-node-emissions trie)))
            (fcn (compile nil
                          (lambda (node)
                            (ig-suffix-weighting node trie root-entropy)))))
       (compute-suffix-weights trie fcn)))
    (inv-ent
     (compute-suffix-weights trie (lambda (node)
                                    (inverse-entropy-suffix-weighting node))))))

(defun compute-suffix-weights (node lambda)
  (funcall lambda node)
  (loop
      for v in  (lash-values (lm-tree-node-children node))
      do (compute-suffix-weights v lambda)))

(defun reweight-suffix-tries (hmm lambda)  
  (maplash (lambda (k v)
             (declare (ignore k))
             (let ((*suffix-trie-root* v))               
               (compute-suffix-weights v lambda)))
           (hmm-suffix-tries hmm)))

(defun diff-entropy-suffix-weighting (node root-entropy &key (alpha 1))
  (declare (ignore alpha))
  (let ((sum (lash-table-sum (lm-tree-node-emissions node))))
    (declare (type single-float root-entropy))
    (if (> sum 0)
        (setf (lm-tree-node-weight node) 
          (max (- root-entropy (lash-table-entropy (lm-tree-node-emissions node))) 0.0))
      (setf (lm-tree-node-weight node)
        nil))))

(defun ig-suffix-weighting (node root-node root-entropy)
  ;; IG = H(x) + P(f)-H(x|f) + P(not-f)-H(x|not-f)
  ;; P(not-f) = 1 - P(f)
  ;; f = node
  ;; x = tags
  (with-slots (emissions total) node
    (let* ((total-ems (lash-table-sum emissions))
           (global-ems (lm-tree-node-adds root-node)))
      (if (not (or (= 0 total-ems) (eq node root-node) (= total-ems global-ems)))
        (loop
            with p-f = (/ total-ems (lm-tree-node-adds root-node))
            with p-not-f = (- 1 p-f)
            with positive-gain = (* -1 p-f (lash-table-entropy emissions))
            for k in (lash-keys (lm-tree-node-emissions root-node))
            for em fixnum = (getlash k (lm-tree-node-emissions root-node))
            for adjusted-em of-type single-float  = (float (- em (getlash k emissions 0)))
            for p-adj-em-not-f = (/ adjusted-em (- global-ems total-ems))
            unless (or (zerop p-adj-em-not-f) (null p-adj-em-not-f))
            summing (* p-adj-em-not-f (log p-adj-em-not-f 2)) into sum
            finally             
              (setf (lm-tree-node-weight node)
                      (+ root-entropy positive-gain (* p-not-f sum))))
        (setf (lm-tree-node-weight node) nil)))))

(defun inverse-entropy-suffix-weighting (node)
  (setf (lm-tree-node-weight node)     
    (* (/ 1 (max (lash-table-entropy (lm-tree-node-emissions node)) 0.08)))))

(defun get-suffix-seqs (form cutoff)
  (coerce (if (> (length form) cutoff)
            (subseq form (- (length form) cutoff))
            form)
          'list))

(defun get-prob-at (suffix tag trie)
  (let* ((node (second (weight-and-dist-of suffix trie)))
         (map (if node (lm-tree-node-emissions node)))
         (sum (if map (lash-table-sum map))))
    (if (and map (/= sum 0))
      (the single-float (/ (getlash tag map 0) sum))
      0)))

;; simple word model which uses distribiution of longest seen suffix
;; current best performer
(defun top-suff-word-model (hmm form)
  (let* ((form (code-to-token form (hmm-token-lexicon hmm)))
         (dist (make-array (hmm-tag-cardinality hmm) :initial-element nil))
         (trie (gethash (get-trie-key form) (hmm-suffix-tries hmm)))
         (suffixes (nreverse (loop for seq on (get-suffix-seqs form *suffix-cutoff*)
                                   collect seq))))
    (loop for i below (hmm-tag-cardinality hmm)
          for prob = (first
                      (last
                       (loop for suffix in suffixes
                             for suff-prob = (get-prob-at suffix i trie)
                             until (= suff-prob 0)
                             collect suff-prob)))
          do (setf (aref dist i) (if prob (log prob) -19.0)))
    dist))

;; TnT style word model with theta coefficient for increasing weight
;; on longer suffixes
(defun tnt-word-model (hmm form &optional (theta-coeff 5))
  (let* ((form (code-to-token form (hmm-token-lexicon hmm)))
         (theta (hmm-theta hmm))
         (dist (make-array (hmm-tag-cardinality hmm) :initial-element nil))
         (trie (gethash (get-trie-key form) (hmm-suffix-tries hmm)))
         (suffixes (nreverse (loop for seq on (get-suffix-seqs form *suffix-cutoff*)
                                   collect seq))))
    (loop for i below (hmm-tag-cardinality hmm)
          do (loop for suffix in suffixes
                   for suff-prob = (get-prob-at suffix i trie)
                   with prob = nil
                   until (= suff-prob 0)
                   do (setf prob
                            (/ (+ (if prob
                                    prob
                                    (aref (hmm-unigram-table hmm) i))
                                  (* theta
                                     suff-prob))
                               (1+ theta)))
                   do (setf theta (* theta theta-coeff))
                   finally (setf (aref dist i) (if prob (log prob) -19.0))))
    dist))

(defun query-suffix-trie (hmm word)
  (declare (optimize (speed 3) (debug 1) (space 0)))
  (let* ((form word)
         (trie-key (get-trie-key form))
         (*suffix-trie-root* (gethash trie-key (hmm-suffix-tries hmm)))
         (nodes (get-suffix-seqs form *suffix-cutoff*))
         (accu-weight 0.0)
         (prob (make-array (hmm-tag-cardinality hmm) :initial-element nil)))
    (declare (type (simple-array t (*)) prob))
    (loop
     for seq in (nreverse (loop for seq on nodes collect seq)) ;; Loop from small to big
     for i fixnum from 0
     for (weight dist) = (weight-and-dist-of seq *suffix-trie-root*)	
     for d-table = (and dist (lm-tree-node-emissions dist))
     for total  = (and dist (lash-table-sum d-table)) ;; FIXME precompute
     when (and weight total)
     do 
     (incf accu-weight weight)
     (loop
      with total of-type single-float = (float total)
      for tag fixnum  in (lash-keys d-table)
      for count of-type single-float = (float (getlash tag d-table))
      for p-t/s of-type single-float  = (/ count total)
      for p-t = (aref (hmm-unigram-table hmm) tag)                      
      when (null (aref prob tag)) do (setf (aref prob tag) p-t) ;; should be P(t) for all corp or P(t) for suffix?
      do (setf (aref prob tag)
               (+ (* (the single-float weight) p-t/s)
                  (the single-float (aref prob tag))))))
    ;;; Bayesian inversion. P(A|B) =  P(B|A)*P(A) / P(B), a = tag, b = suffix
    ;;; Correct P(t) is the unigram prob for the whole model
    ;;; We skip P(B), since it is the same here.
    (loop
     for tag-prob across prob
     for i fixnum  from 0       
     for p-t = (aref (hmm-unigram-table hmm) i)
     when tag-prob do (setf (aref prob i)
                            (/ tag-prob
                               p-t))) 
    ;;; Finally take unknown word probabilities into account:
    (loop
     for i fixnum from 0
     for tag-prob across prob
     if (or (null tag-prob) (>= 0.0 tag-prob))
     do (setf (aref prob i)
              (+ (gethash :unk (aref (hmm-emissions hmm) i) -145.0)
                 -20.0))
     else do (setf (aref prob i) 
                   (+ (gethash :unk (aref (hmm-emissions hmm) i) -5.7)
                      (log tag-prob )))) ;; and convert to log prob
    prob))

