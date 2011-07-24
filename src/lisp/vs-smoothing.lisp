(in-package :mulm)

;;; Some hash-table vs-utils

(defun squared-sum (hash)
  "Computes the squared sum of a vector"
  (loop for i being the hash-values of hash
      sum (expt i 2)))

(defun euclidean-length (hash)
  "Computes the norm of a vector in Euclidean space."
  (sqrt (squared-sum hash)))

(defun dot-product (vec1 vec2)
  "Compute the dot-product of two sparse vectors represented as hash-tabels"
  (loop 
      with lvec1 = (euclidean-length vec1)
      with lvec2 = (euclidean-length vec2)
      with sum = 0
      for i being the hash-keys in vec1
      for x being the hash-values in vec1
      for y = (gethash i vec2 0)
      when (or (= 0 lvec1) (= 0 lvec2)) do (return 0)
      do (incf sum (* x y))
      finally (return (/ sum (* lvec1 lvec2)))))

(defun normalize-vector (hash)
  (loop with sum2 = (squared-sum hash)
        for j being the hash-keys of hash
        using (hash-value n)
        do (setf (gethash j hash)
                 (sqrt (/ (expt n 2) sum2))))
  hash)

(defstruct fv
  id
  (sparse-rep (make-hash-table :size 45))
  filled-rep
  size)

(defstruct vs
  (similarity-fn #'dot-product)
  proximity-matrix
  tag-card
  (vectors (make-hash-table :size 45))
  forward-context
  backward-context)

(defun normalize-vs (vs)
  (loop for vec being the hash-values in (vs-vectors vs)
        do (normalize-vector (fv-sparse-rep vec)))
  vs)



(defstruct symat edge-length internal-vector)


(defun symat-ref (symat i j)
  (let ((idx (- (+ (* (symat-edge-length symat) (min i j)) (max i j))
                (/ (+ (expt (min i j) 2) (min i j)) 2))))
    (aref (symat-internal-vector symat) idx)))

(defun symat-setf (symat i j value)
  (let ((idx (- (+ (* (symat-edge-length symat) (min i j)) (max i j))
                (/ (+ (expt (min i j) 2) (min i j)) 2))))
    (setf (aref (symat-internal-vector symat) idx) value)))


(defsetf symat-ref symat-setf)


(defun mk-symat (edge-length)
  (let ((size (/ (+ (expt edge-length 2) edge-length) 2))
        (symat (make-symat :edge-length edge-length)))
    (setf (symat-internal-vector symat) (make-array size))
    symat))

(defun compute-proximities (space)
  (loop
      with l = (hash-table-count (vs-vectors space))
      with vectors = (vs-vectors space)
      with symat = (mk-symat l)	       
      for fst being the hash-values in vectors
      for i from 0
      when fst do
        (loop 
            for sec being the hash-values in vectors
            for j from 0
            when sec do
              (setf (symat-ref symat i j ) 
                (funcall 
                 (vs-similarity-fn space) 
                 (fv-sparse-rep fst)
                 (fv-sparse-rep sec))))
      finally (setf (vs-proximity-matrix space) symat)))

(defun encode-position (tag offset vs)
  (+ tag
     (* (vs-tag-card vs) offset)))

(defun register-tag-sequence (current-tag context vs)
  (loop
      with vector = (get-or-add current-tag (vs-vectors vs)
                                (make-fv :id current-tag))
      with rep = (fv-sparse-rep vector)
      for tag in context
      for offset from 0
      do (incf (gethash (encode-position tag offset vs) rep 0))))

(defun contextify-tag-sequence (sequence forward-context backward-context)
  (loop
      with forward-queue = (initialize-instance (make-instance 'lru-queue) 
                                                :size forward-context)
      with backward-queue = (initialize-instance (make-instance 'lru-queue) 
                                                 :size backward-context)
      initially (loop 
                    for tag in (rest sequence)
                    for i from 0 below forward-context             
                    do (enqueue forward-queue tag))
      for tag in sequence
      for i from forward-context
      collect (list tag 
                    (append (queue->list forward-queue)
                            (queue->list backward-queue)))
      do (enqueue backward-queue tag)
      when (< i (length sequence))
      do (enqueue forward-queue (elt sequence i))))
                            
(defun make-vs-from-ll-corpus (ll-tags hmm)
  (loop
      with vs = (make-vs :tag-card (hmm-n hmm))
      for seq in ll-tags
      for coded-seq = (mapcar (lambda (x) (tag-to-code hmm x)) seq)
      for contexified = (contextify-tag-sequence coded-seq 3 4)
      do (loop
             for (tag context) in contexified
             do (register-tag-sequence tag context vs))
      finally (normalize-vs vs) (return vs)))


(defun make-vs-counts (ll-tags hmm vs)
  (loop
      with l1 = (hmm-lambda-1 *hmm*)
      with l2 = (hmm-lambda-2 *hmm*)
      with l3 = (hmm-lambda-3 *hmm*)
      with tokens = (hmm-token-count hmm)
      with unigrams = (make-array (hmm-n hmm) :initial-element 0.0)
      with bigrams = (make-array (list (hmm-n hmm) (hmm-n hmm)) :initial-element 0.0)
      with trigrams = (make-array (list (hmm-n hmm) (hmm-n hmm) (hmm-n hmm)) :initial-element 0.0)
      for seq in ll-tags                 
      do
        (loop
            for c1 in seq
            for t1 = (tag-to-code hmm c1)
            do (incf (aref unigrams t1)))
        (loop 
             for (c1 c2) in (partition seq 2)
             for t1 = (tag-to-code hmm c1)
             for t2 = (tag-to-code hmm c2)
             do (incf (aref bigrams t1 t2)))                
         (loop
             for (c1 c2 c3) in (partition seq 3)
             for t1 = (tag-to-code hmm c1)
             for t2 = (tag-to-code hmm c2)
             for t3 = (tag-to-code hmm c3)
             do (incf (aref trigrams t1 t2 t3))
                (loop
                    for sister-tag below (hmm-n hmm)
                    for sim = (symat-ref (vs-proximity-matrix vs) sister-tag t3)
                    when (and (numberp sim) (> sim 0.0))
                    do (incf (aref trigrams t1 t2 sister-tag) sim)
                       (incf (aref bigrams t1 t2) sim)
                       (incf (aref bigrams t2 sister-tag) sim)
                       (incf (aref unigrams t1) sim)
                       (incf (aref unigrams t2) sim)
                       (incf (aref unigrams sister-tag) sim)
                       (incf tokens (* 3 sim))))
      finally
        (format t "renormalizing")
        (loop
            for i fixnum below (hmm-n hmm)
            do (loop 
                   for j fixnum below (hmm-n hmm)
                   for dem-tri = (aref bigrams i j)
                   for dem-bi = (aref unigrams j)
                   do (loop
                          for k fixnum below (hmm-n hmm)
                          for trigram of-type single-float = (aref trigrams i j k)
                          for bigram of-type single-float = (aref bigrams j k)
                          for unigram of-type single-float = (aref unigrams k)
                          do
                            (cond
                             ((and (> trigram 1.0) (> dem-tri 1.0))
                              (setf (aref trigrams i j k)
                                (log (+ (* (/ unigram tokens) l1)
                                        (* (/ bigram dem-bi) l2)
                                        (* (/ trigram dem-tri) l3)))))
                             ((and (> dem-bi 1.0) (> unigram 1.0))
                              (setf (aref trigrams i j k)
                                (log (+ (* (/ unigram tokens) l1)
                                        (* (/ bigram dem-bi l2))))))
                             ((> unigram 1.0)
                              (setf (aref trigrams i j k)
                                (log (* (/ unigram tokens) l1))))
                             (t
                              (setf (aref trigrams i j k)
                                most-negative-single-float))))))

              
        (return trigrams)))
