(in-package :cl-user)

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

;;; ACL does some trick here that makes declaring these single-float slower

(defmacro transition-probability (hmm previous current)
  ;;
  ;; give a tiny amount of probability to unseen transitions
  ;;
  `(the single-float (or (aref (hmm-transitions ,hmm) ,previous ,current) -14.0)))

(defmacro emission-probability (hmm state form)
  `(the single-float (or (gethash ,form (aref (hmm-emissions ,hmm) ,state)) -14.0)))

(defun read-corpus (file &optional (n 100))
  (with-open-file (stream file :direction :input)
    (loop
        with n = (+ n 2)
        with hmm = (make-hmm)
        with transitions = (make-array (list n n))
        with emissions = (make-array n)
        initially
          (loop
              for i from 0 to (- n 1)
              do (setf (aref emissions i) (make-hash-table)))
        for previous = (tag-to-code hmm "<s>") then current
        for line = (read-line stream nil)
        for tab = (position #\tab line)
        for form = (subseq line 0 tab)
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
            for j from 1 to (- n 1)
            for count = (aref transitions i j)
            when count do (setf (aref transitions i j) (float (log (/ count total)))))
        (loop
            with map = (aref (hmm-emissions hmm) i)
            for code being each hash-key in map
            for count = (gethash code map)
            when count do (setf (gethash code map) (float (log (/ count total))))))
  hmm)

(defun viterbi (hmm input)
  (declare (optimize (speed 3) (debug  0) (space 0)))
  (let* ((n (hmm-n hmm))
         (l (length input))
         (viterbi (make-array (list n l) :initial-element most-negative-single-float))
         (pointer (make-array (list n l) :initial-element nil)))
    ;;; Array initial element is not specified in standard, so we carefully
    ;;; specify what we want here. ACL and SBCL usually fills with nil and 0 respectively.
    (loop
        with form = (first input)
        for state fixnum from 1 to (- n 1)
        do
          (setf (aref viterbi state 0)
            (+ (transition-probability hmm 0 state)
               (emission-probability hmm state form)))
          (setf (aref pointer state 0) 0))
    (loop
      for form fixnum in (rest input)
      for time fixnum from 1 to (- l 1)
        do
	(loop
	    for current fixnum from 1 to (- n 1)
              do
	      (loop
		  for previous fixnum from 1 to (- n 2)
		  for old single-float  = (aref viterbi current time)
		  for new  single-float  =
		    (+ (the single-float (aref viterbi previous (- time 1)))
		       (the single-float (transition-probability hmm previous current))
		       (emission-probability hmm current form))
		  when (> new old) do
		    (setf (aref viterbi current time) new)
		    (setf (aref pointer current time) previous))))
    (loop
	with final = (tag-to-code hmm "</s>")
	with time = (- l 1)
        for previous fixnum from 1 to (- n 1)
        for old = (aref viterbi final time)
        for new = (+ (aref viterbi previous time)
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
        for i fixnum from time downto 1
        for state = (aref pointer last i) then (aref pointer state i)
        do (push (elt tags state) result)
        finally (return result))))

(defun evaluate-hmm (hmm file)
  (with-open-file (stream file :direction :input)
    (loop
        with total = 0 with correct = 0
        with forms with tags
	for i from 1
        for line = (read-line stream nil)
        for tab = (position #\tab line)
        for form = (subseq line 0 tab)
	for code = (symbol-to-code form)
        for tag = (and tab (subseq line (+ tab 1)))
        while line
        when (and form tag) do
          (push code forms)
          (push tag tags)
        else do
          (loop
              for gold in (nreverse tags)
              for tag in (viterbi hmm (nreverse forms))
              do (incf total)
              when (string= gold tag) do (incf correct))
          (setf forms nil) (setf tags nil)
        finally (return (/ correct total)))))

  
