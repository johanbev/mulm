(in-package :mulm)

(defun hash-table-diff-msg (h1 h2 &key (msg "diff at"))
  (let ((keys (union (loop for k being the hash-keys of h1 collect k)
                     (loop for k being the hash-keys of h2 collect k))))
    (loop for k in keys
          for v1 = (gethash k h1)
          for v2 = (gethash k h2)
          when (not (= v1 v2))
          do (format t "~a ~a: ~a - ~a~%" msg k v1 v2))))

(defun hmm-model-diff (hmm1 hmm2)
  (when (/= (hmm-token-count hmm1) (hmm-token-count hmm2))
    (format t "Token count diff: ~a - ~a~%"
            (hmm-token-count hmm1) (hmm-token-count hmm2)))
  (when (/= (hmm-tag-cardinality hmm1) (hmm-tag-cardinality hmm2))
    (format t "n diff: ~a - ~a~%"
           (hmm-tag-cardinality hmm1) (hmm-tag-cardinality hmm2)))
  (when (/= (hmm-lambda-1 hmm1) (hmm-lambda-1 hmm2))
    (format t "Lambda-1 diff: ~a - ~a~%"
            (hmm-lambda-1 hmm1) (hmm-lambda-1 hmm2)))
  (when (/= (hmm-lambda-2 hmm1) (hmm-lambda-2 hmm2))
    (format t "Lambda-2 diff: ~a - ~a~%"
            (hmm-lambda-2 hmm1) (hmm-lambda-2 hmm2)))
  (when (/= (hmm-lambda-3 hmm1) (hmm-lambda-3 hmm2))
    (format t "Lambda-3 diff: ~a - ~a~%"
            (hmm-lambda-3 hmm1) (hmm-lambda-3 hmm2)))
  (when (/= (hmm-theta hmm1) (hmm-theta hmm2))
    (format t "Theta diff: ~a - ~a~%"
            (hmm-theta hmm1) (hmm-theta hmm2)))
  (when (/= (hmm-bigram-d hmm1) (hmm-bigram-d hmm2))
    (format t "Bigram-d diff: ~a - ~a~%"
            (hmm-bigram-d hmm1) (hmm-bigram-d hmm2)))
  (when (/= (hmm-trigram-d hmm1) (hmm-trigram-d hmm2))
    (format t "Bigram-d diff: ~a - ~a~%"
            (hmm-trigram-d hmm1) (hmm-trigram-d hmm2)))
  
  (lexicon-diff (hmm-tag-lexicon hmm1) (hmm-tag-lexicon hmm2))
  (lexicon-diff (hmm-token-lexicon hmm1) (hmm-token-lexicon hmm2))

  (loop for i from 0 below (hmm-tag-cardinality hmm1)
        do (loop for j below (hmm-tag-cardinality hmm1)
                 for v1 = (aref (hmm-transitions hmm1) i j)
                 for v2 = (aref (hmm-transitions hmm2) i j)
                 when (and (not (and (null v1) (null v2)))
                           (/= v1 v2))
                 do (format t "Transitions diff at ~a ~a: ~a - ~a~%" i j v1 v2)))

  (when (not (equal (array-dimensions (hmm-emissions hmm1)) (array-dimensions (hmm-emissions hmm2))))
             (format t "Emissions diff array inconsistency: ~a - ~a~%"
                     (array-dimensions (hmm-emissions hmm1)) (array-dimensions (hmm-emissions hmm2))))
  
  (loop for i from 0
        for map1 across (hmm-emissions hmm1)
        for map2 across (hmm-emissions hmm2)
        do (when (/= (hash-table-count map1) (hash-table-count map2))
             (format t "Emissions diff hash table inconsistency at ~a~%" i))
        do (hash-table-diff-msg map1 map2 :msg (format nil "emissions diff for ~a" i)))

  (loop for i from 0 below (hmm-tag-cardinality hmm1)
        do (loop for j from 0 below (hmm-tag-cardinality hmm1)
                 do (loop for k from 0 below (hmm-tag-cardinality hmm1)
                          for v1 = (aref (hmm-trigram-probs hmm1) i j k)
                          for v2 = (aref (hmm-trigram-probs hmm2) i j k)
                          when (and (not (and (null v1) (null v2)))
                                    (/= v1 v2))
                          do (format t "Trigram table diff at ~a ~a ~a: ~a - ~a~%" i j k v1 v2))))

  (loop for i from 0 below (hmm-tag-cardinality hmm1)
        for v1 = (aref (hmm-unigram-probs hmm1) i)
        for v2 = (aref (hmm-unigram-probs hmm2) i)
        when (and (not (and (null v1) (null v2)))
                  (/= v1 v2))
        do (format t "Unigram table diff at ~a: ~a - ~a~%" i v1 v2)))
