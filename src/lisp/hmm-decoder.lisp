(in-package :mulm)

(defvar *bigrams* nil)

(defun viterbi-trigram (hmm input &key (bigrams *bigrams*) (beam-width 15.0))
  (declare (optimize (speed 3) (debug  0) (space 0)))
  (let* ((n (hmm-n hmm))
         (nn (* n n))
         (l (length input))
         (viterbi (make-array (list nn l) :initial-element most-negative-single-float :element-type 'single-float))
         (pointer (make-array (list nn l) :initial-element nil))
         (final most-negative-single-float)
         (final-back nil)
         (end-tag (tag-to-code hmm "</s>"))
         (start-tag (tag-to-code hmm "<s>")))
    ;;; Array initial element is not specified in standard, so we carefully
    ;;; specify what we want here. ACL and SBCL usually fills with nil and 0 respectively.
    (declare (type (simple-array single-float (* *)) viterbi)
             (type (simple-array t (* *)) pointer))
    (declare (type fixnum n nn l start-tag end-tag)
             (type single-float final))    
    (declare (type single-float beam-width))
    (loop 
        with form = (first input)
        with unk = (eql :unk (gethash form *known-codes* :unk))
        with unk-emi = (and unk (query-suffix-trie hmm form))
        for tag fixnum from 0 to (- n 1)
        for state fixnum = (+ (the fixnum (* start-tag n)) tag)
        do (setf (aref viterbi state 0)
             (+ (the single-float (if bigrams
                                      (aref bigrams start-tag tag)
                                    (transition-probability hmm start-tag tag
                                                            :order 1 :smoothing :deleted-interpolation)))
                  (if unk
                      (aref unk-emi tag)
                    (emission-probability hmm tag form))))
        do (setf (aref pointer state 0) 0))
    (loop 
        for form in (rest input)
        for time fixnum from 1 to (- l 1)
        for unk = (eql :unk (gethash form *known-codes* :unk))
        for unk-emi = (and unk (query-suffix-trie hmm form))
        with indices = (hmm-beam-array hmm)
        initially (setf (fill-pointer indices) 0)
                  (loop 
                      for tag below n
                      for x = (+ (the fixnum (* start-tag n)) tag)                                  
                      do (vector-push x indices))
        for best-hypothesis of-type single-float = most-negative-single-float
        for trigger of-type single-float = most-negative-single-float
        do (loop for current fixnum from 0 to (- n 1)
               do (loop
                      for index from 0 below (fill-pointer indices)
                      for previous = (aref indices index)
                      for prev-prob of-type single-float = (aref viterbi previous (- time 1))
                      with old of-type single-float = (aref viterbi current time)
                      with emission of-type single-float = (if unk
                                                               (aref unk-emi current)
                                                             (emission-probability hmm current form))
                      when (> prev-prob old)                       
                      do (multiple-value-bind (t1 t2)
                         (truncate previous n)
                       (declare (type fixnum t1 t2))
                       (let ((new (+ prev-prob
                                     emission
                                     (tri-cached-transition hmm t1 t2 current))))
                         (declare (type single-float new))
                         (when (> new trigger)
                          (setf trigger new)
                          (setf best-hypothesis (- new beam-width)))
                         (when (> new old)
                           (setf old new)
                           (setf (aref viterbi (the fixnum (+ (the fixnum (* t2 n)) current)) time) new)
                           (setf (aref pointer (the fixnum (+ (the fixnum (* t2 n)) current)) time) previous))))))
           (loop
               initially (setf (fill-pointer indices) 0)
               for current of-type fixnum from 0 below nn
               for prob of-type single-float = (the single-float (aref viterbi current time))
               when (> prob best-hypothesis)
               do (vector-push current indices)))
    (loop
        with time fixnum = (1- l)
        for previous fixnum from 0 below nn
        for t1 fixnum = (truncate previous n)
        for t2 fixnum = (rem previous n)
        for new of-type single-float = (+ (aref viterbi previous time)
                                          (tri-cached-transition hmm t1 t2 end-tag))
        when (> new final)
        do (setf final new)
        and do (setf final-back previous))
    (loop with time = (1- l)
        with last = final-back
        with result = (list (code-to-bigram hmm last))
        for i fixnum from time downto 1
        for state = (aref pointer last i) then (aref pointer state i)
        do (push (code-to-bigram hmm state) result)
        finally (return
                    (mapcar #'second result)))))

;;; hvor conser denne?
(defun viterbi-bigram (hmm input &key (beam-width 13.80))
  (declare (optimize (speed 3) (debug  0) (space 0)))
  (let* ((n (hmm-n hmm))
         (l (length input))
         (viterbi (make-array (list n l) :initial-element most-negative-single-float :element-type 'single-float))
         (pointer (make-array (list n l) :initial-element nil)))
    ;;; Array initial element is not specified in standard, so we carefully
    ;;; specify what we want here. ACL and SBCL usually fills with nil and 0 respectively.
    (declare (type fixnum n l))
    (declare (type (simple-array single-float (* *)) viterbi)
             (type (simple-array t (* *)) pointer))
    (loop
        with form of-type fixnum = (first input)
        for state of-type fixnum from 0 to (- n 1)
        with unk = (eql :unk (gethash form *known-codes* :unk))
        with unk-emi = (and unk (query-suffix-trie hmm form))
        do
          (setf (aref viterbi state 0)
            (+ (bi-cached-transition hmm (tag-to-code hmm "<s>") state)
               (if unk
                   (aref unk-emi state)
                 (emission-probability hmm state form))))
          (setf (aref pointer state 0) 0))
    (loop
        for form of-type fixnum in (rest input)
        for time of-type fixnum from 1 to (- l 1)
        for unk = (eql :unk (gethash form *known-codes* :unk))
        for unk-emi = (and unk (query-suffix-trie hmm form))
        with indices = (hmm-beam-array hmm)
        initially (setf (fill-pointer indices) 0)
                  (loop 
                      for x below n
                      do (vector-push x indices))
        for best-hypothesis of-type single-float = most-negative-single-float
        for trigger of-type single-float = most-negative-single-float
        do
          (loop
              for current of-type fixnum from 0 to (- n 1)
              do
                (loop
                    with old of-type single-float = (aref viterbi current time)
                    with emission of-type single-float = (if unk
                                                             (aref unk-emi current)
                                                           (emission-probability hmm current form))
                    for index fixnum from 0 to (1- (fill-pointer indices))
                    for previous = (aref indices index)
                    for prev-prob of-type single-float = (aref viterbi previous   (- time 1))
                    when (> prev-prob old) do
                      (let ((new
                             (+ prev-prob
                                (bi-cached-transition hmm previous current)
                                emission)))
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
                                          (bi-cached-transition hmm previous final))
        when (> new old) do
          (setf (aref viterbi final time) new)
          (setf (aref pointer final time) previous))
    (if (null (aref pointer (tag-to-code hmm "</s>") (- l 1)))
        nil
      (loop
          with final = (tag-to-code hmm "</s>")
          with time = (- l 1)
          with last  = (aref pointer final time)
          with tags = (hmm-tags hmm)
          with result = (list (elt tags last))
          for i of-type fixnum from time downto 1
          for state = (aref pointer last i) then (aref pointer state i)
          never (null state)
          do (push (elt tags state) result)
          finally (return result)))))
