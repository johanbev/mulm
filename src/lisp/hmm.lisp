(in-package :mulm)


(defvar *hmm*)

(defstruct hmm
  tags
  (n 0)
  transitions
  emissions)

(defun tag-to-code (hmm tag)
  (let ((code (position tag (hmm-tags hmm) :test #'string=)))
    (unless code
      (setf (hmm-tags hmm) (append (hmm-tags hmm) (list tag)))
      (setf code (hmm-n hmm))
      (incf (hmm-n hmm)))
    code))

(defmethod print-object ((object hmm) stream)
  (format stream "<HMM with ~a states>" (hmm-n object)))

(defmacro transition-probability (hmm previous current)
  ;;
  ;; give a tiny amount of probability to unseen transitions
  ;;
  `(the single-float (or (aref (hmm-transitions ,hmm) ,previous ,current) -14.0)))

(defmacro emission-probability (hmm state form)
  `(the single-float (or (gethash ,form (aref (the (simple-array t *) (hmm-emissions ,hmm)) ,state)) -14.0)))

#| OLD CODE
(defun read-corpus (file &optional (n 100))
  (with-open-file (stream file :direction :input)
    (loop
        with n = (+ n 2)
        with hmm = (make-hmm)
        with transitions = (make-array (list n n) :initial-element nil)
        with emissions = (make-array n :initial-element nil)
        initially
          (loop
              for i from 0 to (- n 1)
              do (setf (aref emissions i) (make-hash-table)))
        for previous = (tag-to-code hmm "<s>") then current
        for line = (read-line stream nil)
        for tab = (position #\tab line)
        for form = (normalize-token (subseq line 0 tab))
	for code = (symbol-to-code form)
        for tag = (if tab (subseq line (+ tab 1)) "</s>")
        for current = (tag-to-code hmm tag)
        for map = (aref emissions current)
        while line
        when (and form (not (string= form ""))) do 
          (if (gethash code map)
            (incf (gethash code map))
            (setf (gethash code map) 1))
        do
          (if (aref transitions previous current)
            (incf (aref transitions previous current))
            (setf (aref transitions previous current) 1))
        when (string= tag "</s>") do (setf current (tag-to-code hmm "<s>"))
        finally
          (setf (hmm-transitions hmm) transitions)
          (setf (hmm-emissions hmm) emissions)
          (return hmm))))
|#

(defun partition (list &optional (len 2))
  "Partitions the list into ordered sequences of len consecutive elements."
  (loop for i on list
        for j below (- (length list) (1- len))
        collect (subseq i 0 len)))

(defun train (corpus &optional (n 100))
  "Trains a HMM model from a corpus (a list of lists of word/tag pairs)."
  (loop with n = (+ n 2)
        with hmm = (make-hmm)
        with transitions = (make-array (list n n) :initial-element nil)
        with emissions = (make-array n :initial-element nil)
        initially (loop for i from 0 to (- n 1)
                        do (setf (aref emissions i) (make-hash-table)))

        for sentence in corpus
        do (let ((bigrams (partition (append '("<s>")
                                             (mapcar #'second sentence)
                                             '("</s>")))))
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
                        (setf (aref transitions previous current) 1))))
                
        finally
          (setf (hmm-transitions hmm) transitions)
          (setf (hmm-emissions hmm) emissions)
          (return (train-hmm hmm))))

(defun train-hmm (hmm)
  (loop
      with transitions = (hmm-transitions hmm)
      with n = (hmm-n hmm)
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
            (+ (transition-probability hmm 0 state)
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
