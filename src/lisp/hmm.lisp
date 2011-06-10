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

(defun transition-probability (hmm previous current)
  ;;
  ;; give a tiny amount of probability to unseen transitions
  ;;
  (or (aref (hmm-transitions hmm) previous current) -14))

(defun emission-probability (hmm state form)
  (gethash form (aref (hmm-emissions hmm) state) -14))

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
              do (setf (aref emissions i) (make-hash-table :test #'equal)))
        for previous = (tag-to-code hmm "<s>") then current
        for line = (read-line stream nil)
        for tab = (position #\tab line)
        for form = (subseq line 0 tab)
        for tag = (if tab (subseq line (+ tab 1)) "</s>")
        for current = (tag-to-code hmm tag)
        for map = (aref emissions current)
        while line
        when (and form (not (string= form ""))) do 
          (if (gethash form map)
            (incf (gethash form map))
            (setf (gethash form map) 1))
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
      for i from 0 to (- n 2)
      for total = (loop
                      for j from 0 to (- n 1)
                      sum (or (aref transitions i j) 0))
      do
        (loop
            for j from 1 to (- n 1)
            for count = (aref transitions i j)
            when count do (setf (aref transitions i j) (log (/ count total))))
        (loop
            with map = (aref (hmm-emissions hmm) i)
            for form being each hash-key in map
            for count = (gethash form map)
            when count do (setf (gethash form map) (log (/ count total)))))
  hmm)

;(defparameter *eisner* (train-hmm (read-corpus "eisner.tt" 2)))

(defun viterbi (hmm input)
  (let* ((n (hmm-n hmm))
         (l (length input))
         (viterbi (make-array (list n l) :initial-element most-negative-single-float))
         (pointer (make-array (list n l))))
    (loop
        with form = (first input)
        for state from 1 to (- n 1)
        do
          (setf (aref viterbi state 0)
            (+ (transition-probability hmm 0 state)
               (emission-probability hmm state form)))
          (setf (aref pointer state 0) 0))
    (loop
        for form in (rest input)
        for time from 1 to (- l 1)
        do
          (loop
              for current from 1 to (- n 1)
              do
                (loop
                    for previous from 1 to (- n 2)
                    for old = (aref viterbi current time)
                    for new = (+ (aref viterbi previous (- time 1))
                                 (transition-probability hmm previous current)
                                 (emission-probability hmm current form))
                    when (> new old) do
                      (setf (aref viterbi current time) new)
                      (setf (aref pointer current time) previous))))
    (loop
	with final = (tag-to-code hmm "</s>")
	with time = (- l 1)
        for previous from 1 to (- n 1)
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
        for i from time downto 1
        for state = (aref pointer last i) then (aref pointer state i)
        do (push (elt tags state) result)
        finally (return result))))

(defun evaluate-hmm (hmm file)
  (with-open-file (stream file :direction :input)
    (loop
        with total = 0 with correct = 0
        with forms with tags
        for line = (read-line stream nil)
        for tab = (position #\tab line)
        for form = (subseq line 0 tab)
        for tag = (and tab (subseq line (+ tab 1)))
        while line
        when (and form tag) do
          (push form forms)
          (push tag tags)
        else do
          (loop
              for gold in (nreverse tags)
              for tag in (viterbi hmm (nreverse forms))
              do (incf total)
              when (string= gold tag) do (incf correct))
          (setf forms nil) (setf tags nil)
        finally (return (/ correct total)))))
