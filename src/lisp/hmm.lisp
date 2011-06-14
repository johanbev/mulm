(in-package :mulm)


(defvar *hmm*)

(defstruct hmm
  tags
  (n 0)
  transitions
  emissions
  trigram-table)

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

(defmacro transition-probability (hmm previous current)
  ;;
  ;; give a tiny amount of probability to unseen transitions
  ;;
  `(the single-float (or (aref (hmm-transitions ,hmm) ,previous ,current) -14.0)))

(defmacro emission-probability (hmm state form)
  `(the single-float (or (gethash ,form (aref (the (simple-array t *) (hmm-emissions ,hmm)) ,state)) -14.0)))

(defun partition (list &optional (len 2))
  "Partitions the list into ordered sequences of len consecutive elements."
  (loop for i on list
        for j below (- (length list) (1- len))
        collect (subseq i 0 len)))

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
        initially (loop for i from 0 to (- n 1)
                        do (setf (aref emissions i) (make-hash-table)))

        for sentence in corpus
        do (let* ((tags (append '("<s>")
                                (mapcar #'second sentence)
                                '("</s>")))
                  (bigrams (partition tags 2))
                  (trigrams (partition tags 3)))
             (loop for (code tag) in sentence
                   do (incf (gethash code
                                     (aref emissions
                                           (tag-to-code hmm tag))
                                     0)))
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
          (return (train-hmm hmm))))

(defun train-hmm (hmm)
  (let ((n (hmm-n hmm)))
    (loop
      with transitions = (hmm-transitions hmm)
      for i from 0 to (- n 1)
      for total = (loop
                      for j from 0 to (- n 1)
                      sum (or (aref transitions i j) 0))
      do
        (loop
            for j from 0 to (- n 1)
            for count = (aref transitions i j)
            when count do (setf (aref transitions i j) (float (log (/ count total)))))
        (loop
            with map = (aref (hmm-emissions hmm) i)
            for code being each hash-key in map
            for count = (gethash code map)
            when count do (setf (gethash code map) (float (log (/ count total))))))

    (loop for k from 0 below n
          for total = (let ((sum 0))
                        (loop for i from 0 below n
                              do (loop for j from 0 below n
                                       for count = (aref (hmm-trigram-table hmm) i j k)
                                       when count
                                       do (incf sum count)))
                        sum)
          do (loop for i from 0 below n
                   do (loop for j from 0 below n
                            for count = (aref (hmm-trigram-table hmm) i j k)
                            when count
                            do (setf (aref (hmm-trigram-table hmm) i j k)
                                     (float (log (/ count total))))))))
  
  hmm)

(defun viterbi (hmm input)
  #+:allegro (declare (:explain :variables :types))
  (declare (optimize (speed 3) (debug  0) (space 0)))
  (let* ((n (hmm-n hmm))
         (l (length input))
         (viterbi (make-array (list n l) :initial-element most-negative-single-float))
         (pointer (make-array (list n l) :initial-element nil)))
    ;;; Array initial element is not specified in standard, so we carefully
    ;;; specify what we want here. ACL and SBCL usually fills with nil and 0 respectively.
    (declare (type fixnum n l))
    (loop
        with form of-type fixnum = (first input)
        for state of-type fixnum from 0 to (- n 1)
        do
          (setf (aref viterbi state 0)
            (+ (transition-probability hmm (tag-to-code hmm "<s>") state)
               (emission-probability hmm state form)))
          (setf (aref pointer state 0) 0))
    (loop
      for form of-type fixnum in (rest input)
      for time of-type fixnum from 1 to (- l 1)
        do
	(loop
	    for current of-type fixnum from 0 to (- n 1)
              do
	      (loop
		  with old of-type single-float = (aref viterbi current time)
		  for previous of-type fixnum from 0 to (- n 1)
		  for prev-prob of-type single-float = (aref viterbi previous (- time 1))
		  when (> prev-prob old) do
		    (let ((new
			   (+ prev-prob
			      (transition-probability hmm previous current)
			      (emission-probability hmm current form))))
		      (declare (type single-float new))
		      (when (> new old)
			(setf old new)
			(setf (aref viterbi current time) new)
			(setf (aref pointer current time) previous))))))
    (loop
	with final = (tag-to-code hmm "</s>")
	with time of-type fixnum = (- l 1)
        for previous of-type fixnum from 0 to (- n 1)
        for old of-type single-float = (aref viterbi final time)
        for new of-type single-float = (+ (the single-float (aref viterbi previous time))
                     (transition-probability hmm previous final))
        when (or (null old) (> new old)) do
          (setf (aref viterbi final time) new)
          (setf (aref pointer final time) previous))
    (loop
	with final = (tag-to-code hmm "</s>")
	with time = (- l 1)
        with last = (aref pointer final time)
        with tags = (hmm-tags hmm)
        with result = (list (elt tags last))
        for i of-type fixnum from time downto 1
        for state = (aref pointer last i) then (aref pointer state i)
        do (push (elt tags state) result)
        finally (return result))))

(defun beam-viterbi (hmm input &key (beam-width 1.807))
  (declare (optimize (speed 3) (debug  0) (space 0)))
  (setf beam-width (float beam-width))
  (let* ((n (hmm-n hmm))
         (l (length input))
         (viterbi (make-array (list n l) :initial-element most-negative-single-float))
         (pointer (make-array (list n l) :initial-element nil)))
    ;;; Array initial element is not specified in standard, so we carefully
    ;;; specify what we want here. ACL and SBCL usually fills with nil and 0 respectively.
    (declare (type fixnum n l)
	     (type single-float beam-width))
    (loop
        with form of-type fixnum = (first input)
        for state of-type fixnum from 0 to (- n 1)
        do
          (setf (aref viterbi state 0)
            (+ (transition-probability hmm 0 state)
               (emission-probability hmm state form)))
          (setf (aref pointer state 0) 0))
    (loop
	for form of-type fixnum in (rest input)
	for time of-type fixnum from 1 to (- l 1)
	with indices = (make-array n :initial-contents (loop for i from 0 to (1- n) collect i) :fill-pointer t)
	for best-hypothesis of-type single-float = most-negative-single-float
	for trigger of-type single-float = most-negative-single-float
        do
	  (loop	     
	      for current of-type fixnum from 0 to (1- n)
	      do
		(loop
		    with old of-type single-float = (aref viterbi current time)
		    with prev-time fixnum = (1- time)
		    for index fixnum from 0 to (1- (fill-pointer indices))
		    for previous = (aref indices index)
		    for prev-prob of-type single-float = (aref viterbi previous prev-time)
		    when (> prev-prob old)
		    do
		      (let ((new (+ prev-prob
				    (the single-float (transition-probability hmm previous current))
				    (emission-probability hmm current form))))
			(declare (type single-float new))
			(when (> new trigger)
			  (setf trigger new)
			  (setf best-hypothesis (- new beam-width)))
			(when (> new old)
			  (setf old new)
			  (setf (aref viterbi current time) new)
			  (setf (aref pointer current time) previous)))))
	  (loop
	      initially (setf (fill-pointer indices) 0)
	      for current of-type fixnum from 0 to (- n 1)
	      for prob of-type single-float = (the single-float (aref viterbi current time))
	      when (> prob best-hypothesis)
	      do (vector-push current indices)))
    (loop
	with final = (tag-to-code hmm "</s>")
	with time of-type fixnum = (- l 1)
        for previous of-type fixnum from 0 to (- n 1)
        for old of-type single-float = (aref viterbi final time)
        for new of-type single-float = (+ (the single-float (aref viterbi previous time))
                     (transition-probability hmm previous final))
        when (or (null old) (> new old)) do
          (setf (aref viterbi final time) new)
          (setf (aref pointer final time) previous))
    (loop
	with final = (tag-to-code hmm "</s>")
	with time = (- l 1)
        with last = (aref pointer final time)
        with tags = (hmm-tags hmm)
        with result = (list (elt tags last))
        for i of-type fixnum from time downto 1
        for state = (aref pointer last i) then (aref pointer state i)
        do (push (elt tags state) result)
        finally (return result))))
