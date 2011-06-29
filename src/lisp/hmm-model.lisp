(in-package :mulm)

(defvar *hmm*)

(defparameter *estimation-cutoff* 0)

(defparameter *known-codes* (make-hash-table))

(defstruct hmm
  tag-array
  beam-array
  tags
  (n 0)
  transitions
  emissions
  trigram-table
  unigram-table
  token-count
  (lambda-1 0.0 :type single-float)
  (lambda-2 0.0 :type single-float)
  (lambda-3 0.0 :type single-float)
  (suffix-tries (make-hash-table :test #'equal))
  theta
  current-transition-table)

(defun tag-to-code (hmm tag)
  (let ((code (position tag (hmm-tags hmm) :test #'string=)))
    (unless code
      (setf (hmm-tags hmm) (append (hmm-tags hmm) (list tag)))
      (setf code (hmm-n hmm))
      (incf (hmm-n hmm)))
    code))

(defun bigram-to-code (hmm bigram)
  "Returns index of bigram data in "
  (let ((c1 (tag-to-code hmm (first bigram)))
        (c2 (tag-to-code hmm (second bigram))))
    (+ (* c1 (hmm-n hmm))
       c2)))

(defmethod print-object ((object hmm) stream)
  (format stream "<HMM with ~a states>" (hmm-n object)))

(defun transition-probability (hmm previous current &key (order 1) (smoothing :constant))
  (declare (type fixnum current order))
  (declare (type hmm hmm))
  ;;
  ;; give a tiny amount of probability to unseen transitions
  ;;
  (the single-float
    (log
     (float
      (cond ((and (eql order 1) (eql smoothing :constant))
             (or (aref (the (simple-array t (* *)) (hmm-transitions hmm)) previous current) 0.000001))
            ((and (= order 1) (eql smoothing :deleted-interpolation))
             (+
                  (* (the single-float (hmm-lambda-1 hmm))
                       (or (aref (the (simple-array t (*)) (hmm-unigram-table hmm)) current)
                           0.0))
                (the single-float
                  (* (hmm-lambda-2 hmm)
                     (or (aref (the (simple-array t (* *)) (hmm-transitions hmm)) previous current)
					     0.0)))))
            ((and (= order 2) (eql smoothing :simple-back-off))
               (or (aref (the (simple-array  t (* * *))
                           (hmm-trigram-table hmm))
                         (first previous) (second previous) current)
                   (aref (the (simple-array t (* *)) (hmm-transitions hmm))
                         (second previous) current)
                   (aref (the (simple-array t (*))
                           (hmm-unigram-table hmm))
                         current)
                   0.00000001))
            ((and (= order 2) (eql smoothing :deleted-interpolation))
             (+ (* (hmm-lambda-1 hmm)
                   (or (aref (the (simple-array t (*)) (hmm-unigram-table hmm)) current)
                       0.0))
                (* (hmm-lambda-2 hmm)
                   (or (aref (the (simple-array t (* *)) (hmm-transitions hmm)) (second previous) current)
                       0.0))
                (* (hmm-lambda-3 hmm)
                   (or (aref (the (simple-array t (* * *)) (hmm-trigram-table hmm)) (first previous) (second previous)
                             current)
                       0.0))))
            (t (error "What!")))))))

(defun make-transition-table (hmm order smoothing)
  (setf (hmm-current-transition-table hmm)
    (let ((tag-card (hmm-n hmm)))
      (cond
       ((= order 1)
	(let* ((table (make-array (list tag-card tag-card) :element-type 'single-float :initial-element 0.0)))
	  (loop
	      for i from 0 below tag-card
	      do (loop
                 for j below tag-card do
                   (setf (aref table i j) (transition-probability hmm i j :order 1 :smoothing smoothing))))
	  table))
       ((= order 2)
        (let* ((table (make-array (list tag-card tag-card tag-card) 
                                  :element-type 'single-float :initial-element most-negative-single-float)))
          (loop
              for i from 0 below tag-card
              do (loop 
                     for j  from 0 below tag-card
                     do (loop
                            for k from 0 below tag-card do
                              (setf (aref table i j k) 
                                (transition-probability hmm (list i j) k :order 2 :smoothing smoothing)))))
          table))))))

(defmacro bi-cached-transition (hmm from to)
  `(the single-float  
     (aref (the (simple-array single-float (* *)) (hmm-current-transition-table ,hmm)) ,from ,to)))

(defmacro tri-cached-transition (hmm t1 t2 to)
  `(the single-float
     (aref (the (simple-array single-float (* * *)) (hmm-current-transition-table ,hmm)) ,t1 ,t2 ,to)))

(defmacro emission-probability (hmm state form)
  `(the single-float (or (gethash ,form (aref (the (simple-array t (*)) (hmm-emissions ,hmm)) ,state)) -19.0)))

(defun train (corpus &optional (n nil))
  "Trains a HMM model from a corpus (a list of lists of word/tag pairs)."

  ;; determine tagset size if not specified by the n parameter
  (when (null n)
    (let ((tag-map (make-hash-table :test #'equal)))
      (loop for sentence in corpus
            do (loop for token in sentence
                     do (if (not (gethash (second token) tag-map))
                          (setf (gethash (second token) tag-map) t))))
      (setf n (hash-table-count tag-map))))
  
  (loop with n = (+ n 2)
        with hmm = (make-hmm)
        with transitions = (make-array (list n n) :initial-element nil)
        with emissions = (make-array n :initial-element nil)
        with trigram-table = (make-array (list n n n) :initial-element nil)
        with unigram-table = (make-array n :initial-element nil)
        with token-count = 0
        initially (loop for i from 0 to (- n 1)
                        do (setf (aref emissions i) (make-hash-table)))

        for sentence in corpus
        do (let* ((tags (append '("<s>")
                                (mapcar #'second sentence)
                                '("</s>")))
                  (unigrams tags)
                  (bigrams (partition tags 2))
                  (trigrams (partition tags 3)))
             (loop for (code tag) in sentence
                 do (incf token-count)
                 do (setf (gethash code *known-codes*) t)
                 do (incf (gethash code
                                   (aref emissions
                                         (tag-to-code hmm tag))
                                   0)))

             (loop for unigram in unigrams
                   for code = (tag-to-code hmm unigram)
                   do (if (aref unigram-table code)
                        (incf (aref unigram-table code))
                        (setf (aref unigram-table code) 1)))
             
             (loop for bigram in bigrams
                   for previous = (tag-to-code hmm (first bigram))
                   for current = (tag-to-code hmm (second bigram))
                   do (if (aref transitions previous current)
                        (incf (aref transitions previous current))
                        (setf (aref transitions previous current) 1)))

             (loop for trigram in trigrams
                   do (let ((t1 (tag-to-code hmm (first trigram)))
                            (t2 (tag-to-code hmm (second trigram)))
                            (t3 (tag-to-code hmm (third trigram))))
                        (if (aref trigram-table t1 t2 t3)
                          (incf (aref trigram-table t1 t2 t3))
                          (setf (aref trigram-table t1 t2 t3) 1)))))
        
        finally
          (setf (hmm-transitions hmm) transitions)
          (setf (hmm-emissions hmm) emissions)
          (setf (hmm-trigram-table hmm) trigram-table)
          (setf (hmm-unigram-table hmm) unigram-table)
          (setf (hmm-token-count hmm) token-count)
          (setf (hmm-beam-array hmm) (make-array n :fill-pointer t))
          (setf (hmm-tag-array hmm)
                (make-array n :element-type 'fixnum 
                            :initial-contents (loop for i from 0 to (1- n) collect i)))
          (calculate-deleted-interpolation-weights hmm)
          (calculate-theta hmm)           
          (train-hmm hmm corpus)
          (return hmm)))

(defun calculate-theta (hmm)
  (let* ((total (loop for count across (hmm-unigram-table hmm)
                      summing count))
         (probs (loop for count across (hmm-unigram-table hmm)
                      collect (float (/ count total))))
         (mean (/ (loop for p in probs
                        summing p)
                  (hmm-n hmm))))
    (setf (hmm-theta hmm)
          (float (* (/ 1.0 (1- (hmm-n hmm)))
                    (loop 
                        for p in probs
                        summing (expt (- p mean) 2)))))))


(defun add-to-suffix-tries (hmm  word tag count)
  (let* ((form (code-to-symbol word))
         (trie-key (capitalized-p form))
         (lookup (gethash trie-key (hmm-suffix-tries hmm))))
    (when (null lookup)
      (setf lookup (make-lm-tree-node))
      (setf (gethash trie-key (hmm-suffix-tries hmm))
            lookup))
    (add-word word tag  count lookup)))

(defun total-emissions (code hmm)
  (loop
      for table across (hmm-emissions hmm)
      summing (gethash code table 0)))

(defun build-suffix-tries (hmm)
  (loop 
      for state below (hmm-n hmm)
      for emmission-map = (aref (hmm-emissions hmm) state)
       do
        (loop 
            for word being the hash-keys in emmission-map
            for count = (gethash word emmission-map)
            when (and (<= count 10) (<= (total-emissions word hmm) 10)
                      (not (eql word :unk)))                       
            do (add-to-suffix-tries hmm word state count)))
  (maphash (lambda (k v)
             (declare (ignore k))
             (compute-suffix-weights v))
           (hmm-suffix-tries hmm)))

(defun calculate-deleted-interpolation-weights (hmm)
  (let ((lambda-1 0)
        (lambda-2 0)
        (lambda-3 0)
        (n (hmm-n hmm)))
    (loop 
	for i from 0 below n
	do 
	  (loop 
	      for j from 0 below n
	      do (loop 
		     for k from 0 below n
		     for tri-count = (aref (hmm-trigram-table hmm) i j k)
		     when tri-count
		     do 
		       (let* ((bi-count (aref (hmm-transitions hmm) j k))
			      (uni-count (aref (hmm-unigram-table hmm) k))
			      (c1 (let ((bi-count (aref (hmm-transitions hmm) i j)))
				    (if (and bi-count
					     (> bi-count 1)
					     (> bi-count *estimation-cutoff*))
					(/ (1- tri-count)
					   (1- bi-count))
                                              0)))
			      (c2 (let ((uni-count (aref (hmm-unigram-table hmm) j)))
				    (if (and uni-count
					     (> uni-count 1)
					     (> uni-count *estimation-cutoff*))
					(/ (1- bi-count)
					   (1- uni-count))
				      0)))
			      (c3 (/ (1- uni-count)
				     (1- (hmm-token-count hmm)))))
			 (cond ((and (>= c3 c2) (>= c3 c1))
				(incf lambda-3 tri-count))
			       ((and (>= c2 c3) (>= c2 c1))
				(incf lambda-2 tri-count))
			       ((and (>= c1 c3) (>= c1 c2))
                                        (incf lambda-1 tri-count))
			       (t (error "What!")))))))
    (let ((total (+ lambda-1 lambda-2 lambda-3)))
      (setf (hmm-lambda-1 hmm) (float (/ lambda-1 total)))
      (setf (hmm-lambda-2 hmm) (float (/ lambda-2 total)))
      (setf (hmm-lambda-3 hmm) (float (/ lambda-3 total)))
      hmm)))

(defun train-hmm (hmm corpus)
  (build-suffix-tries hmm)
  (let ((n (hmm-n hmm)))
    (loop
        with transitions = (hmm-transitions hmm)
        for i from 0 to (- n 1)
        for total = (float (loop
                               for j from 0 to (- n 1)
                               for count = (aref transitions i j)
                               when (and count (> count *estimation-cutoff*))
                               sum count))
        do
          (loop
              for j from 0 to (- n 1)
              for count = (aref transitions i j)
              when (and count (> count *estimation-cutoff*))
              do (setf (aref transitions i j) (float (/ count total))))
          
          (make-good-turing-estimate (aref (hmm-emissions hmm) i)
                                     (hash-table-sum (aref (hmm-emissions hmm) i))
                                     (elt (hmm-tags hmm) i))
          (loop
              with map = (aref (hmm-emissions hmm) i)
              for code being each hash-key in map
              for count = (gethash code map)
              when (and count (> count *estimation-cutoff*))
              do (setf (gethash code map) (float (log (/ count total))))))
    
          
    ;;; this is quite hacky but count-trees will be abstracted nicely
    ;;; in the near future, however, this should estimate correct
    ;;; n-gram probabilities
    (loop
        with *lm-root* = (make-lm-tree-node)
        with *hmm* = hmm
        with count-tree = (build-model (mapcar (lambda (x)
                                                 (mapcar (lambda (x)
                                                           (tag-to-code hmm x))
                                                         x))
                                               (ll-to-tag-list corpus)) 3)
        for t1 from 0 below n
        for t1-node = (gethash t1 (lm-tree-node-children *lm-root*))
        do
          (loop 
              for t2 from 0 below n 
              for t2-node = (gethash t2 (lm-tree-node-children t1-node))
              when t2-node do
                (loop 
                    with total = (lm-tree-node-total t2-node)
                    for t3 from 0 below n
                    for t3-node = (gethash t3 (lm-tree-node-children t2-node))
                    when t3-node do
                      (let ((prob (/ (lm-tree-node-total t3-node)
                                     total)))
                        (setf (aref (hmm-trigram-table hmm) t1 t2 t3)
                          prob)))))
    (loop for i from 0 below n
        for count = (aref (hmm-unigram-table hmm) i)
        with total = (hmm-token-count hmm)
        do (setf (aref (hmm-unigram-table hmm) i)
             (float (/ count total)))))
  
  hmm)

(defun code-to-bigram (hmm bigram)
  (list (elt (hmm-tags hmm) (floor (/ bigram (hmm-n hmm))))
        (elt (hmm-tags hmm) (mod bigram (hmm-n hmm)))))
