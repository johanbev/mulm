(in-package :mulm)

(defstruct lm-tree-node
  weight
  (adds 0)
  (total 0)
  (emissions (make-hash-table :size 11))
  (children (make-hash-table :size 11)))

(defvar *lm-root* (make-lm-tree-node))

(defmethod print-object ((object lm-tree-node) stream)
  (with-slots (total children) object
    (format stream "#<LM-Node ~a decs, ~a total>" (hash-table-count children) total)))

(defun add-sequence (seq &optional (lm-root *lm-root*))
  (with-slots (total children) lm-root
    (incf total)
    (let ((child
           (get-or-add (first seq) children (make-lm-tree-node))))
      (if (rest seq)
          (add-sequence (rest seq) child)
        (incf (lm-tree-node-total child))))))

(defun sentence-to-n-grams (sentence n)
  (let ((buffer (make-instance 'lru-queue)))
    (initialize-instance buffer :size n)
    (loop
        for word in sentence
        for seq = (queue->list (enqueue buffer word))
        do (add-sequence seq)
        finally (mapl #'add-sequence (rest seq)))))

(defun build-model (sentences n)
  (loop 
      for sent in sentences
      do (sentence-to-n-grams (mapcar (lambda (x) (tag-to-code *hmm* x))
				      (append 
					      (list "<s>")
					      sent
					      (list  "</s>" ))) n)))
(defun create-kn-count-tree (corpus n)
  (setf *lm-root* (make-lm-tree-node))
  (build-model (ll-to-tag-list corpus) n))


(defparameter *kn-d* 1.16) ;; should be found empirically!

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
	     for adjusted-count = (max (- second-count *kn-d*) 0)
	     for bigram-prob = (+ (/ adjusted-count first-count) ;; adjusted estimate
				  (* (/ *kn-d* first-count)
				     first-decs
				     (aref unigrams second)))
	     do (setf (aref bigram-probs first second) (if (= 0 bigram-prob)
							   most-negative-single-float
							 (log bigram-prob))))
      finally (return bigram-probs)))

(defun find-d ()	     
  (loop
      with *decoder* = #'beam-viterbi
      for *kn-d* in '(1.1 1.12 1.14 1.16 1.18 1.2 1.22 1.24 1.26 1.28 1.3)
      with uni = (kn-unigrams)
      for bigs = (kn-bigrams uni)
      do (format t "~&D: ~a" *kn-d*)
      do (setf (hmm-current-transition-table *hmm*) bigs)
	 (do-evaluation)))
