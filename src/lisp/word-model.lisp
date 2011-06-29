(in-package :mulm)

(defvar *suffix-trie-root*
    (make-lm-tree-node))

(defparameter *suffix-adds* 0)
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


(defun weight-suffix-trie-node (node)
  (setf (lm-tree-node-weight node) 
    (* (/ 1 (max (hash-table-entropy (lm-tree-node-emissions node)) 0.2)))))

(defun compute-suffix-weights (node)
  (when (> (hash-table-count (lm-tree-node-emissions node)) 0)
    (weight-suffix-trie-node node))
  (maphash
   (lambda (k v)
     (declare (ignorable k))
     (compute-suffix-weights v))
   (lm-tree-node-children node)))


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


(defun query-suffix-trie (hmm word)
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
    (loop
        for seq in (reverse (loop for seq on nodes collect seq)) ;;loop from small to big
        for i from 0
        for (weight dist) = (weight-and-dist-of seq *suffix-trie-root*)	
        for d-table = (and dist (lm-tree-node-emissions dist))
        for total = (and dist (hash-table-sum d-table)) ;; FIXME precompute
        when weight	     
        do 
          (incf accu-weight  weight)
          (loop            
              for tag being the hash-keys in d-table
              for count = (float (gethash tag d-table))
              for p-t/s = (/ count total)
              for p-t = (aref (hmm-unigram-table hmm) tag) ;; check
              when (null (aref prob tag)) do (Setf (aref prob tag) p-t)
              do (setf (aref prob tag)
                   (/ (+ (* weight p-t/s) (* theta (aref prob tag)))
                      (1+ theta)))))
    (loop
        for tag-prob across prob
        for i from 0       
        for p-t =  (aref (hmm-unigram-table hmm) i) ;; check
        when tag-prob do (setf (aref prob i)
                           (/ tag-prob 
                              p-t)))
                   
    (loop
        for i from 0
        for tag-prob across prob
        if (or (null tag-prob) (>= 0.0 tag-prob)) do
          (setf (aref prob i)
            (+ (gethash :unk (aref (hmm-emissions hmm) i) -5.7)
               -19.37))    
        else do (setf (aref prob i) 
             (+ (gethash :unk (aref (hmm-emissions hmm) i) -5.7)
               (log tag-prob ))))
    prob))

;; 80.73% 87.71  86.16%


