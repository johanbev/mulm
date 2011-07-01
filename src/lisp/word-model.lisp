(in-package :mulm)

(defvar *suffix-trie-root*
    (make-lm-tree-node))

(defparameter *suffix-cutoff* 10)

(defun add-word (word tag count node)
  (let* ((form  (code-to-symbol word))
         (nodes (coerce (if (> (length form) *suffix-cutoff*)
                            (subseq form (- (length form)  *suffix-cutoff*))
                          form)
                        'list))
         (sufs (* count (length nodes))))
    (incf (lm-tree-node-adds node) count)
    (incf (gethash tag (lm-tree-node-emissions node) 0) count)
    (loop 
        for seq on nodes
        do (add-word-to-trie seq tag (abs count) node))))

;;; Add the word (character sequence) to the trie. When we reach the end of the
;;; sequence we add the emission
(defun add-word-to-trie (word tag count &optional (node *suffix-trie-root*))
  (declare (type lm-tree-node node)
           (fixnum count tag))
  (with-slots (total children emissions)
      node
    (incf total count)
    (let* ((rest (rest word))
           (first (first word))
           (child-node (get-or-add first children (make-lm-tree-node))))
      (if rest
          (add-word-to-trie rest tag count (get-or-add first children (make-lm-tree-node)))
        (progn
          (incf (lm-tree-node-total child-node) count)
          (incf (gethash tag (lm-tree-node-emissions child-node) 0) count))))))

(defun weight-and-dist-of (word node)
  "Walk the suffix trie and return the weight of the end node"
  (let* ((rest (rest word))
         (first (first word))
         (child (gethash first (lm-tree-node-children node))))
    (if (null child)
        nil
      (progn
        (if (null rest)
            (list (lm-tree-node-weight child)
                  child)
          (weight-and-dist-of rest child))))))

(defparameter *default-suffix-weighting*
    (lambda (node)
      (diff-entropy-suffix-weighting 
       node 
       (hash-table-entropy (lm-tree-node-emissions *suffix-trie-root*)))))

(defun compute-suffix-weights (node &key (fcn *default-suffix-weighting*))
  (funcall fcn node)
  (maphash
   (lambda (k v)
     (declare (ignorable k))
     (compute-suffix-weights v :fcn fcn))
   (lm-tree-node-children node)))

(defun reweight-suffix-tries (hmm lambda)  
  (maphash (lambda (k v)
             (declare (ignore k))
             (let ((*suffix-trie-root* v))               
               (compute-suffix-weights v :fcn lambda)))
           (hmm-suffix-tries hmm)))

(defun diff-entropy-suffix-weighting (node root-entropy &key (alpha 1))
  (let ((sum (hash-table-sum (lm-tree-node-emissions node))))
    (if (> sum 0)
        (setf (lm-tree-node-weight node) 
          (max (- root-entropy (renyi-entropy (lm-tree-node-emissions node) alpha)) 0.0))
      (setf (lm-tree-node-weight node)
        nil))))

(defun ig-suffix-weighting (node root-node root-entropy)
  ;; IG = H(x) + P(f)-H(x|f) + P(not-f)-H(x|not-f)
  ;; P(not-f) = 1 - P(f)
  ;; f = node
  ;; x = tags
  (with-slots (emissions total) node
    (let* ((total-ems (hash-table-sum emissions))
           (global-ems (lm-tree-node-adds root-node)))
      (if (not (or (= 0 total-ems) (eq node root-node) (= total-ems global-ems)))
        (loop
            with p-f = (/ total-ems (lm-tree-node-adds root-node))
            with p-not-f = (- 1 p-f)
            with positive-gain = (* -1 p-f (hash-table-entropy emissions))
            for k being the hash-keys in (lm-tree-node-emissions root-node)
            for em fixnum = (gethash k (lm-tree-node-emissions root-node))
            for adjusted-em of-type single-float  = (float (- em (gethash k emissions 0)))
            for p-adj-em-not-f = (/ adjusted-em (- global-ems total-ems))
            unless (or (zerop p-adj-em-not-f) (null p-adj-em-not-f))
            summing (* p-adj-em-not-f (log p-adj-em-not-f 2)) into sum
            finally             
              (setf (lm-tree-node-weight node)
                      (+ root-entropy positive-gain (* -1 p-not-f sum))))
        (setf (lm-tree-node-weight node) nil)))))

(defun inverse-entropy-suffix-weighting (node)
  (setf (lm-tree-node-weight node)     
    (* (/ 1 (max (hash-table-entropy (lm-tree-node-emissions node)) 0.08)))))

(defun query-suffix-trie (hmm word)
  (declare (optimize (speed 3) (debug 0) (space 0)))
  (let* ((form (code-to-symbol word))
         (trie-key (capitalized-p form))
         (*suffix-trie-root* (gethash trie-key (hmm-suffix-tries hmm)))
         (*suffix-adds* (lm-tree-node-adds *suffix-trie-root*))
         (nodes (coerce (if (> (length form) *suffix-cutoff*)
                            (subseq form (- (length form)  *suffix-cutoff*))
                          form)
                        'list))
         (accu-weight 0.0)
         (theta (hmm-theta hmm))
         (prob (make-array (hmm-n hmm) :initial-element nil)))
    (declare (type (simple-array t (*)) prob))
    (declare (dynamic-extent nodes))
    (loop
        for seq in (nreverse (loop for seq on nodes collect seq)) ;; Loop from small to big
        for i fixnum from 0
        for (weight dist) = (weight-and-dist-of seq *suffix-trie-root*)	
        for d-table = (and dist (lm-tree-node-emissions dist))
        for total  = (and dist (hash-table-sum d-table)) ;; FIXME precompute
        when (and weight total)
        do 
          (incf accu-weight  weight)
          (loop
              with total of-type single-float = (float total)
              for tag fixnum being the hash-keys in d-table
              for count of-type single-float = (float (gethash tag d-table))
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
        if (or (null tag-prob) (>= 0.0 tag-prob)) do
          (setf (aref prob i)
            (+ (gethash :unk (aref (hmm-emissions hmm) i) -5.7)
               -19.37))    
        else do (setf (aref prob i) 
             (+ (gethash :unk (aref (hmm-emissions hmm) i) -5.7)
                (log tag-prob )))) ;; and convert to log prob
    prob))

