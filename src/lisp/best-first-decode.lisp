(in-package :mulm)

(defstruct node
  time
  value
  (probability most-negative-single-float)
  backpointer)

(defmethod print-object (( object node) stream)
  (format stream "<N ~a <- ~a : ~a>" 
	  (node-value object)
	  (node-backpointer object)
	  (node-probability object)))

(defun node-heap-key (node &optional new-probability)
  (if new-probability
      (setf (node-probability node) new-probability)
    (node-probability node)))

(defmacro make-all-transitions (hmm from observation &key limit-array)
  `(loop
      with time fixnum = (1+  (node-time ,from))
      for n fixnum from 0 below (hmm-n ,hmm)
      for prob of-type single-float = (+ (the single-float (node-probability ,from))
					 (transition-probability ,hmm (node-value ,from)  n)
					 (emission-probability ,hmm n ,observation))
      when (or (not ,limit-array)
	       (> prob (the single-float (aref ,limit-array time n))))
      collect (make-node
		     :time time
		     :value n
		     :probability prob
		     :backpointer ,from)))

(defun trellis-best-first (hmm input)
  (declare (optimize (speed 3) (space 0) (debug 0)))
  ;; First allocate the trellis and agenda
  (let* ((length (length input))
	 (input (apply #'vector input))
	 (tag-card (hmm-n hmm))
	 (trellis (make-array (list length tag-card)))
	 (limit-array (make-array (list length tag-card)
				  :element-type 'single-float :initial-element most-negative-single-float))
	 (agenda (make-instance 'cl-heap:fibonacci-heap :key #'node-heap-key :sort-fun #'>))
	 (start-node (make-node :time -1 :probability 0.0 :value (tag-to-code hmm "<s>"))))
    ;; make transitions from the start node and enqueue
    (loop
	for node in (make-all-transitions hmm start-node (elt input 0))
	do (multiple-value-bind (it index)
	       (cl-heap:add-to-heap agenda node)
	     (setf (aref trellis (node-time node) (node-value node)) index)
	     (setf (aref limit-array (node-time node) (node-value node)) (node-probability node))))    
    (loop
	for next = (cl-heap:pop-heap agenda)
	while next
	when (and (= (tag-to-code hmm "</s>") (node-value next))
		  (= length (node-time next))) do
	  ;; first node to be dequeued at the end with end-tag is best
	  (return (backtrack hmm next))
	when (= (node-time next) (1- length)) do
	  (cl-heap:add-to-heap agenda (make-node :time (1+ (node-time next))
						 :value (tag-to-code hmm "</s>")
						 :probability (+ (the single-float (node-probability next))
								 (transition-probability hmm (node-value next)
											 (tag-to-code hmm "</s>")))
						 :backpointer next)) ;; pre-end nodes enqueue an end node
	else do
	     (loop 
		 for node in (make-all-transitions hmm next (elt input (1+ (node-time next)))
						   :limit-array limit-array)
		 when (> (the single-float (node-probability node))
			 (the single-float (aref limit-array (node-time node) (node-value node)))) do
		   (setf (aref limit-array (node-time node) (node-value node)) (node-probability node))
		   (if (aref trellis (node-time node) (node-value node))
		       (cl-heap:decrease-key agenda 
					     (aref trellis (node-time node) (node-value node)) 
					     (node-probability node))
		     (multiple-value-bind (it index)
			 (cl-heap:add-to-heap agenda node)
		       (setf (aref trellis (node-time node) (node-value node)) index)))))))


(defun backtrack (hmm node)
  (loop
      with path = nil
      with node = (node-backpointer node)
      while node
      for value = (elt (hmm-tags hmm) (node-value node))
      do (push value path)
	 (setf node (node-backpointer node))
      finally (return (rest path))))
