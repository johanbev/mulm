(in-package :mulm)


;; TODO maybe do the encoded forms as a struct for speed
(defun encode-input (hmm input)
  "Encodes the input to the integer codes of the token lexicon of the HMM model. If a word is not
   in the lexicon it is encoded as a list containing the :unk keyword and the token.

   returns a list of encoded tokens."
  (loop for token in input
        for code = (token-to-code token (hmm-token-lexicon hmm) :rop t)
        if code collect code
        else collect (list :unk token)))

(defun unknown-token-p (hmm token)
  (declare (ignore hmm))
  (and (listp token) (eql (first token) :unk)))


(defmacro encode-bigram (t1 t2 &optional (n 'n))
  `(the fixnum (+ (the fixnum (* ,t1 ,n)) ,t2)))

(defvar *warn-if-long* nil)

(defun viterbi-trigram (hmm input)
  (declare (optimize (speed 3) (debug 1)))
  (let* ((input (encode-input hmm input))
         (n (hmm-n hmm))
         (nn (* n n))
         (l (length input))
         (pointer (if (< l 99)
                      (first (hmm-caches hmm))
                    (make-array (list nn l) :initial-element nil)))
         (viterbi (if (< l 99)
                      (second (hmm-caches hmm))
                    (make-array (list nn l) :initial-element most-negative-single-float :element-type 'single-float)))
         (active-tags (make-array (list n l) :initial-element nil))
         (final most-negative-single-float)
         (final-back nil)
         (end-tag (token-to-code "</s>" (hmm-tag-lexicon hmm) :rop t))
         (start-tag (token-to-code "<s>" (hmm-tag-lexicon hmm) :rop t))
         (previous-possible (third (hmm-caches hmm)))
         (next-possible (fourth (hmm-caches hmm))))
    ;;; Array initial element is not specified in standard, so we carefully
    ;;; specify what we want here. ACL and SBCL usually fills with nil and 0 respectively.
    (declare (type (simple-array single-float (* *)) viterbi)
             ;; this type declaration fails on sbcl
             ; (type (simple-array boolean (* *)) pointer active-tags)
             (type (array fixnum (*)) previous-possible next-possible))
    (declare (type fixnum n nn l start-tag end-tag)
             (type single-float final))
    (setf (fill-pointer previous-possible) 0
          (fill-pointer next-possible) 0)
    (when (and *warn-if-long* (> l 100))
      (format t "~&Decoding a long sequence"))
    ;; LW6 can't handle enormous allocations on the stack
    (loop 
        with form = (first input)
        with unk = (unknown-token-p hmm form)
        with unk-emi = (and unk (query-suffix-trie hmm (second form)))
        for tag fixnum from 0 to (- n 1)
        for state = (+ (* start-tag n) tag)
        do (setf (aref viterbi state 0)
                 (+ (the single-float (if (hmm-bigram-transition-table hmm)
                                          (bi-cached-transition hmm start-tag tag)
                                        (transition-probability hmm start-tag tag
                                                                :order 1 :smoothing :deleted-interpolation)))
                    (the single-float
                      (if unk
                          (aref unk-emi tag)
                        (emission-probability hmm tag form)))))
        do (setf (aref pointer state 0) 0))
    (loop                                                        
        initially (loop 
                      for x fixnum below n
                      for state = (encode-bigram start-tag x)
                      do (vector-push state previous-possible)
                         (setf (aref active-tags x 0) t))
                  
        for form in (rest input)
        for time fixnum from 1 to (- l 1)
        for previous-time fixnum = (1- time)
        for unk = (unknown-token-p hmm form)
        for unk-emi = (and unk 
                           (query-suffix-trie hmm (second form)))
                                                                   
        do 
          (loop
              with touch = nil
              for current fixnum from 0 to (- n 1)
              for emission of-type single-float = (if unk
                                                       (aref unk-emi current)
                                                     (emission-probability hmm current form))
              when (or  (> emission -19.0)
                        (and unk (> emission -100.10)))
              
              ;;; This tag can generate current emission:     
              do (setf touch t)
                 (setf (aref active-tags current time) t)
                 (loop
                     for tag fixnum below n
                     when (aref active-tags tag previous-time)
                     do (vector-push (encode-bigram tag current) next-possible))
                 (loop
                     for tag fixnum below n ;; blank out eventual old stuff in trellis
                     do (setf (aref viterbi (encode-bigram tag current) time) most-negative-single-float))
                 (loop
                     ;;; the loop of death, we really don't want to go here if we can spare it
                   
                     for previous fixnum across previous-possible ;;; for each possible previous tag
                     for prev-prob of-type single-float = (aref viterbi previous previous-time)
                     with old of-type single-float = most-negative-single-float
                                                     
                     when (> prev-prob old) ;; this "stupid" litte optimization is important in big tagsets                          
                     do (multiple-value-bind (t1 t2)
                            (truncate previous n)
                          (declare (type fixnum t1 t2))
                          (let ((new (+ prev-prob
                                        emission
                                        (tri-cached-transition hmm t1 t2 current))))
                            (declare (type single-float new))
                            (when (> new old)
                              (setf old new)
                              (setf (aref viterbi (encode-bigram t2 current) time) new)
                              (setf (aref pointer (encode-bigram t2 current) time) previous)))))
              finally
                (psetf previous-possible next-possible
                       next-possible previous-possible)
                (setf (fill-pointer next-possible) 0)
                (unless touch
                  (error "No tag generates current emission!"))))        
    (loop
        with end fixnum = (1- l)
        for code fixnum across previous-possible                        
        do
          (multiple-value-bind (t1 t2)
              (truncate code n)
            (declare (type fixnum t1 t2))
            (let* ((prob (aref viterbi code end)))
              (declare (type fixnum code)
                       (type single-float prob))
              (when (> prob final)
                (let ((new (+ prob
                              (tri-cached-transition hmm t1 t2 end-tag))))
                  (declare (type single-float new))
                  (when (> new final)
                    (setf final new)
                    (setf final-back code)))))))
    
    (loop with time = (1- l)
        with last = final-back
        with result = (list (code-to-bigram hmm last))
        for i fixnum from time downto 1
        for state = (aref pointer last i) then (aref pointer state i)
        do (push (code-to-bigram hmm state) result)
        finally (return
                    (mapcar #'second result)))))

;; NOTE transition tables must be cached by calling make-transition-table() before calling this function
(defun viterbi-bigram (hmm input &key (beam-width 13.80) &allow-other-keys)
  (declare (optimize (speed 3) (debug  1) (space 0)))
  (let* ((input (encode-input hmm input))
         (n (hmm-n hmm))
         (l (length input))
         (viterbi (make-array (list n l) :initial-element most-negative-single-float :element-type 'single-float))
         (pointer (make-array (list n l) :initial-element nil)))
    ;;; Array initial element is not specified in standard, so we carefully
    ;;; specify what we want here. ACL and SBCL usually fills with nil and 0 respectively.
    (declare (type fixnum n l))
    (declare (type (simple-array single-float (* *)) viterbi)
             (type (simple-array t (* *)) pointer))
    (loop
        with form = (first input)
        for state from 0 to (- n 1)
        with unk = (unknown-token-p hmm form)
        with unk-emi = (and unk (query-suffix-trie hmm (second form)))
        do (setf (aref viterbi state 0)
                 (+ (bi-cached-transition hmm
                                          (token-to-code "<s>" (hmm-tag-lexicon hmm) :rop t)
                                          state)
                    (if unk
                      (aref unk-emi state)
                      (emission-probability hmm state form))))
          (setf (aref pointer state 0) 0))
    (loop
        for form in (rest input)
        for time of-type fixnum from 1 to (- l 1)
        for unk = (unknown-token-p hmm form)
        for unk-emi = (and unk (query-suffix-trie hmm (second form)))
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
        with final = (token-to-code "</s>" (hmm-tag-lexicon hmm) :rop t)
        with time of-type fixnum = (- l 1)
        for previous of-type fixnum from 0 to (- n 1)
        for old of-type single-float = (aref viterbi final time)
        for new of-type single-float = (+ (the single-float (aref viterbi previous time))
                                          (bi-cached-transition hmm previous final))
        when (> new old) do
          (setf (aref viterbi final time) new)
          (setf (aref pointer final time) previous))
    (if (null (aref pointer (token-to-code "</s>" (hmm-tag-lexicon hmm) :rop t) (- l 1)))
        nil
      (loop
          with final = (token-to-code "</s>" (hmm-tag-lexicon hmm) :rop t)
          with time = (- l 1)
          with last  = (aref pointer final time)
          with result = (list (code-to-token last (hmm-tag-lexicon hmm)))
          for i of-type fixnum from time downto 1
          for state = (aref pointer last i) then (aref pointer state i)
          never (null state)
          do (push (code-to-token state (hmm-tag-lexicon hmm)) result)
          finally (return result)))))

(defstruct viterbi-decoder
  function
  model
  (caches (make-hash-table))
  trellis
  back)

(defun make-decoder (training-corpus)
  (setup-decoder
   (make-viterbi-decoder :function #'viterbi-bigram-slow
                         :model (train training-corpus))))

(defun setup-decoder (decoder &optional input)
  (declare (ignore input))
  (setf (gethash :unknown-word (viterbi-decoder-caches decoder))
        (make-hash-table))
  decoder)

(defun decode-start (decoder hmm input viterbi pointer &optional (constraints nil))
  (let ((n (hmm-n hmm)))
    (loop
     with form = (first input)
     for state from 0 to (- n 1)
     when (or (null constraints) (member state constraints)) 
     do (setf (aref viterbi state 0)
              (+ (bi-cached-transition hmm (token-to-code "<s>" (hmm-tag-lexicon hmm) :rop t) state)
                 (emission-probability-slow decoder hmm state form)))
     and do (setf (aref pointer state 0) 0))))

(defun decode-form (decoder hmm time form viterbi pointer indices trigger
                            beam-width best-hypothesis &optional (constraints nil))
  (let ((n (hmm-n hmm)))
    (loop
     for current of-type fixnum from 0 to (- n 1)
     do (loop
         with old of-type single-float = (aref viterbi current time)
         with emission of-type single-float = (emission-probability-slow decoder hmm current form)
         for index fixnum from 0 to (1- (fill-pointer indices))
         for previous = (aref indices index)
         for prev-prob of-type single-float = (aref viterbi previous   (- time 1))
         when (and (> prev-prob old)
                   (or (null constraints) (member current constraints)))
         do (let ((new
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
     do (vector-push current indices))))

(defun decode-end (hmm viterbi pointer l)
  (let ((n (hmm-n hmm)))
    (loop
     with final = (token-to-code "</s>" (hmm-tag-lexicon hmm) :rop t)
     with time of-type fixnum = (- l 1)
     for previous of-type fixnum from 0 to (- n 1)
     for old of-type single-float = (aref viterbi final time)
     for new of-type single-float = (+ (the single-float (aref viterbi previous time))
                                       (bi-cached-transition hmm previous final))
     when (> new old) do
     (setf (aref viterbi final time) new)
     (setf (aref pointer final time) previous))))

(defun backtrack-slow (hmm pointer l)
  (if (null (aref pointer (token-to-code "</s>" (hmm-tag-lexicon hmm) :rop t) (- l 1)))
    nil
    (loop
     with final = (token-to-code "</s>" (hmm-tag-lexicon hmm) :rop t)
     with time = (- l 1)
     with last  = (aref pointer final time)
     with result = (list (code-to-token last (hmm-tag-lexicon hmm)))
     for i of-type fixnum from time downto 1
     for state = (aref pointer last i) then (aref pointer state i)
     never (null state)
     do (push (code-to-token state (hmm-tag-lexicon hmm)) result)
     finally (return result))))

(defun viterbi-bigram-slow (hmm input &key (beam-width 13.80) (decoder nil) (constraints nil))
  (declare (ignore hmm))
  ; (declare (optimize (speed 3) (debug  1) (space 0)))
  (let* ((hmm (viterbi-decoder-model decoder))
         (input (encode-input hmm input))
         (n (hmm-n hmm))
         (l (length input))
         (constraints (loop for tags in constraints
                            collect (loop for tag in tags
                                          collect (token-to-code tag (hmm-tag-lexicon hmm) :rop t))))
         (viterbi (make-array (list n l)
                              :initial-element most-negative-single-float
                              :element-type 'single-float))
         (pointer (make-array (list n l) :initial-element nil)))
    ;;; Array initial element is not specified in standard, so we carefully
    ;;; specify what we want here. ACL and SBCL usually fills with nil and 0 respectively.
    (declare (type fixnum n l))
    (declare (type (simple-array single-float (* *)) viterbi)
             (type (simple-array t (* *)) pointer))

    ;; nuke existing caches
    (setup-decoder decoder)

    (decode-start decoder hmm input viterbi pointer (first constraints))
    
    (loop
        for form in (rest input)
        for time of-type fixnum from 1 to (- l 1)

        with indices = (hmm-beam-array hmm)
        initially (setf (fill-pointer indices) 0)
                  (loop 
                      for x below n
                      do (vector-push x indices))
        for best-hypothesis of-type single-float = most-negative-single-float
        for trigger of-type single-float = most-negative-single-float

        for constraint = (and constraints (elt constraints time))

        do (decode-form decoder hmm time form viterbi pointer indices
                        trigger beam-width best-hypothesis constraint))

    (decode-end hmm viterbi pointer l)
    
    (backtrack-slow hmm pointer l)))




