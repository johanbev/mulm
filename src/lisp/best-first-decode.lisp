(in-package :mulm)

(defstruct node
  (time 0 :type fixnum)
  (value 0 :type fixnum)
  (probability most-negative-single-float :type single-float)
  backpointer)

(defvar *minimum-transition* 0.0)

(defvar *max-emission-table* )

(declaim (type single-float *minimum-transition*))

(defun find-highest-transition (hmm)
  (loop
      for i from 0 below (hmm-tag-cardinality hmm)
      maximizing  (loop for j from 0 below (hmm-tag-cardinality hmm)
		   maximizing (bi-cached-transition hmm i j))))

(defun fill-max-emissions (hmm)
  (loop
      with table = (make-hash-table)
      for i from 0 below (lexicon-count (hmm-token-lexicon hmm))
      for val = (loop 
		    for j from 0 below (hmm-tag-cardinality hmm)
		    maximizing (emission-probability hmm j i))
      do (setf (gethash i table) val)
      finally (return table)))

(defparameter *emission-array* nil)    

(defun make-emission-array (input)
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (loop
      with accu of-type single-float = 0.0		  
      with emissions of-type (simple-array single-float (*)) = 
	(make-array (length input) :element-type 'single-float :initial-element most-negative-single-float)
      for form fixnum in input
      for i fixnum from 0
      for best-emission of-type single-float  = (gethash form *max-emission-table* -14.0)
      do (incf accu best-emission)
	 (setf (aref emissions i) accu)
      finally (loop for i from 0 below (length input)
		  do (setf (aref emissions i) (- accu (aref emissions i))))
	      (return emissions)))			  

(defmacro hypothesise-cost (current time length)
  `(the single-float
     (+
      (the single-float (aref (the (simple-array single-float (*)) *emission-array*) ,time))
      (the single-float (* (the single-float *minimum-transition*) (float (- ,length ,time))))
      (the single-float ,current))))
  
(defmethod print-object (( object node) stream)
  (format stream "<N ~a <- ~a : ~a>" 
	  (node-value object)
	  (node-backpointer object)
	  (node-probability object)))

(defmacro make-all-transitions (hmm from observation &key limit-array)
  `(loop
       initially (Setf (fill-pointer generation-vector) 1)
       with time fixnum  = (1+  (node-time ,from))
       for n fixnum from 0 below (hmm-tag-cardinality ,hmm)
       for prob of-type single-float = (+ (the single-float (node-probability ,from))
					  (the single-float (bi-cached-transition ,hmm (node-value ,from)  n))
					  (the single-float (emission-probability ,hmm n ,observation)))
       when (or (not ,limit-array)
		(> prob (the single-float (aref ,limit-array time n))))
       do (vector-push  (make-node
			 :time time
			 :value n
			 :probability prob
			 :backpointer ,from)
			generation-vector)))

(defparameter num-nodes 0)

(defun trellis-best-first (hmm input)
  (declare (optimize (speed 3) (space 0) (debug 1)))
  (setf *emission-array* (make-emission-array input))
  (setf num-nodes 0)
  ;; First allocate the trellis and agenda
  (let* ((length (length  input))
	 (input (apply #'vector input))
	 (tag-card (hmm-tag-cardinality hmm))
	 (generation-vector (make-array (1+ tag-card) :fill-pointer t :initial-element nil))
	 (trellis (make-array (list length tag-card) :initial-element nil))
	 (limit-array (make-array (list length tag-card)
				  :element-type 'single-float :initial-element most-negative-single-float))
	 (*heap* (make-heap))
	 (start-node (make-node :time -1 :probability 0.0 :value (token-to-code *start-tag* (hmm-tag-lexicon hmm) :rop t)))
	 (end-tag (token-to-code *start-tag* (hmm-tag-lexicon hmm) :rop t)))
    ;; make transitions from the start node and enqueue
    (declare
       ;; this declaration should be redundant
       ; (type (simple-array single-float (* *)) limit-array)
	     (type (simple-array t (* *)) trellis)
	     (type (simple-array t (*)) input))
    (loop
       initially (make-all-transitions hmm start-node (elt input 0))
       for node  = (vector-pop generation-vector)
       while node
       do (let ((time (node-time node))
		(value (node-value node))
		(probability (node-probability node)))
	    (declare (fixnum time value))
	    (declare (single-float probability))
	    (multiple-value-bind (it)
		(the fixnum (add-to-heap (cons (hypothesise-cost probability time length) node)))
	      (setf (aref trellis time value) it)
	      (setf (aref limit-array time value) probability))))
    (loop
     for next of-type node = (pop-heap)
     while next

        ;; first node to be dequeued at the end with end-tag is best
	      
	when (and (= end-tag (node-value next))
		 (= length (node-time next))) do
	  (return (backtrack hmm next))

	when (= (node-time next) (the fixnum (1- length))) do
	  (let ((prob (+ (node-probability next)
			 (bi-cached-transition hmm (node-value next)
					       end-tag))))
	    (declare (type single-float prob))
	    (add-to-heap (cons prob
			       (make-node :time (the fixnum (1+ (node-time next)))
					  :value end-tag
					  :probability prob
					  :backpointer next))))
	  
	  ;; pre-end nodes enqueue an end node
	else do
	     (loop 
		 initially (make-all-transitions hmm next (elt input (the fixnum (1+ (node-time next))))
						 :limit-array limit-array)
		 for node of-type node = (vector-pop generation-vector)
		 while node
		 when (> (the single-float (node-probability node))
			 (the single-float (aref limit-array (node-time node) (node-value node))))
		 do
		   (setf (aref limit-array (node-time node) (node-value node)) (node-probability node))		   
		   (if (aref trellis (node-time node) (node-value node))
		       (progn 
			 (setf (aref trellis (node-time node) (node-value node))
			   (adjust-priority (aref trellis (node-time node) (node-value node)) 
					    (hypothesise-cost (node-probability node) (node-time node) length)))
			 (setf (cdr (aref (heap-backing-array *heap*)
					  (aref trellis (node-time node)
						(node-value node))))
			   node))			 
		     (multiple-value-bind (it)
			 (add-to-heap (cons (hypothesise-cost (node-probability node) (node-time node) length) node))
		       (setf (aref trellis (node-time node) (node-value node)) it)))))))

(defun backtrack (hmm node)
  (loop
      with path = nil
      with node = (node-backpointer node)
      while node
      for value = (code-to-token (node-value node)
                                 (hmm-tag-lexicon hmm))
      do (push value path)
	 (setf node (node-backpointer node))
      finally (return (rest path))))


(defmacro make-all-trigram-transitions (hmm from observation &key limit-array)
  `(loop
       with time fixnum = (1+  (node-time ,from))
       for n fixnum from 0 below (hmm-tag-cardinality ,hmm)
       for prob of-type single-float = (+ (the single-float (node-probability ,from))
					  (tri-cached-transition ,hmm 
								 (node-value (node-backpointer ,from))
								 (node-value ,from)  n)
					  (emission-probability ,hmm n ,observation))
       when (or (not ,limit-array)
		(> prob (the single-float (aref ,limit-array time n))))
       do (incf num-nodes) and
       collect (make-node
		     :time time
		     :value n
		     :probability prob
		     :backpointer ,from)))
