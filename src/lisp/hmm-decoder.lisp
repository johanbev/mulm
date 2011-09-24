(in-package :mulm)


;; TODO maybe do the encoded forms as a struct for speed
(defun encode-input (hmm input)
  "Encodes the input to the integer codes of the token lexicon of the HMM model. If a word is not
   in the lexicon it is encoded as a list containing the :unk keyword and the token.

   returns a list of encoded tokens."
  (loop for token in input
        for code = (token-to-code token (hmm-token-lexicon hmm))
        if code collect code
        else collect (list :unk token)))

(defun unknown-token-p (hmm token)
  (declare (ignore hmm))
  (and (listp token) (eql (first token) :unk)))


(defmacro encode-bigram (t1 t2 &optional (n 'n))
  "Computes the bigram state-code of two tags"
  ;;; We should probably put this in a macrolet inside viterbi trigram.
  `(the fixnum (+ (the fixnum (* ,t1 ,n)) ,t2)))

(defvar *warn-if-long* nil)

(defstruct decoder-state
  ; viterbi trellis and backpointer array
  (pointer nil :type (simple-array t (* *)))
  (viterbi nil :type (simple-array single-float (* *)))
  ;; Keep track of `active' tags, ie. tags that have P(w|t) > 0.
  (active-tags (make-array '(0 0)) :type (simple-array t (* *)))
  ;; The final sequence probability
  (final most-negative-single-float :type single-float)
  ;; The final backpointer
  (final-back nil)
  (previous-possible (make-array 0 :initial-element 0 :element-type 'fixnum)
                     :type (array fixnum (*)))
  (next-possible (make-array 0 :initial-element 0 :element-type 'fixnum)
                 :type (array fixnum (*))))

(defun setup-trigram-decoding-state (decoder input)
  (let* ((hmm (decoder-model decoder))
         (n (hmm-tag-cardinality hmm))
         (nn (* n n))
         (l (length input))
         (state (make-decoder-state
                 :pointer (make-array (list nn l)
                                      :initial-element nil)
                 :viterbi (make-array (list nn l)
                                      :initial-element most-negative-single-float
                                      :element-type 'single-float)
                 :active-tags (make-array (list n l) :initial-element nil)
                 :final most-negative-single-float
                 :final-back nil
                 :previous-possible (make-array (* n n)
                                                :initial-element 0
                                                :fill-pointer 0
                                                :element-type 'fixnum)
                 :next-possible (make-array (* n n) :initial-element 0 :fill-pointer 0 :element-type 'fixnum))))
    (setf (fill-pointer (decoder-state-previous-possible state)) 0
          (fill-pointer (decoder-state-next-possible state)) 0)

    state))

(defun setup-bigram-decoding-state (decoder input)
  (let* ((hmm (decoder-model decoder))
         (n (hmm-tag-cardinality hmm))
         (l (length input)))
    (make-decoder-state :viterbi (make-array (list n l)
                                             :initial-element most-negative-single-float
                                             :element-type 'single-float)
                        :pointer (make-array (list n l) :initial-element nil))))

(defun viterbi-trigram (decoder input &key &allow-other-keys)
  "Yields the best sequence of hmm states given the observations in input.
   input : list of strings
   returns a list of strings"
  (declare (optimize (speed 3) (debug 1)))
  (let* ((hmm (decoder-model decoder))
         (input (encode-input hmm input)) ;; encode the input to numerical codes
         (n (hmm-tag-cardinality hmm)) ;; get the size of the tag set
         (l (length input))
         (end-tag (token-to-code *end-tag* (hmm-tag-lexicon hmm)))
         (start-tag (token-to-code *start-tag* (hmm-tag-lexicon hmm)))
         (decoding-state (setup-trigram-decoding-state decoder input)))
    (declare (type fixnum n nn l start-tag end-tag))
    
    (when (and *warn-if-long* (> l 100))
      (format t "~&Decoding a long sequence"))
    
    ;; Fill out the first column in the trellis
    (loop
        with form = (first input)
        with unk = (unknown-token-p hmm form)
        with unk-emi = (and unk (query-suffix-trie hmm (second form)))
        for tag fixnum from 0 to (- n 1)
        for state = (+ (* start-tag n) tag)
        do (setf (aref (decoder-state-viterbi decoding-state) state 0)
                 (+ (the single-float
                         ; This is the "correct" transition probobality for the first column
                         ;
                         ; (tri-cached-transition hmm start-tag start-tag tag)
                         ;
                         ; But it actually works slightly better to use the smoothed bigram probability
                         (transition-probability hmm tag start-tag nil :order 1 :smoothing :deleted-interpolation))
                    (the single-float
                      (if unk
                          (aref unk-emi tag)
                        (emission-probability hmm tag form)))))
        do (setf (aref (decoder-state-pointer decoding-state) state 0) 0))
    ;; Fill out all remaining columns
    (loop
        ;; First put all simple unigram states on the agenda:
        initially (loop
                      for x fixnum below n
                      for state = (encode-bigram start-tag x)
                      do (vector-push state (decoder-state-previous-possible decoding-state))
                         (setf (aref (decoder-state-active-tags decoding-state) x 0) t))
                  
        ;; Now we are ready to fill the trellis
        for form in (rest input) ;; For each word form in the rest of the input
        for time fixnum from 1 to (- l 1)
        for previous-time fixnum = (1- time)
        for unk = (unknown-token-p hmm form)
        ;; If this was an unknown token, we get a vector of emission probabilities here:
        for unk-emi = (and unk 
                           (query-suffix-trie hmm (second form)))
                                                                   
        do 
          (loop
              with touch = nil ;; a guard to see if we actually do something at this time in the trellis
              for current fixnum from 0 to (- n 1)
              for emission of-type single-float = (if unk
                                                       (aref unk-emi current)
                                                    (emission-probability hmm current form))
              ;; If the emission probability is too low P(w|t) <= 0, we discard this state from further processing.
              when (or  (> emission -19.0)
                        (and unk (> emission -100.10)))
              
              ;;; This tag can generate current emission:
              do (setf touch t)
                 (setf (aref (decoder-state-active-tags decoding-state) current time) t)
                 (loop
                     for tag fixnum below n
                     ;; Remove old cells in the reused trellis:
                     do (setf (aref (decoder-state-viterbi decoding-state) (encode-bigram tag current) time) most-negative-single-float)
                     ;; If the previous tag n was active we put the combination of that tag and the current tag on the agenda:
                     when (aref (decoder-state-active-tags decoding-state) tag previous-time)
                     do (vector-push (encode-bigram tag current)
                                     (decoder-state-next-possible decoding-state)))
                 
                 ;; Now we do the actual argmaxing:
                 (loop
                     for previous fixnum across (decoder-state-previous-possible decoding-state) ;;; for each possible previous tag
                     ;;; lookup the probability of that cell in the trellis:
                     for prev-prob of-type single-float = (aref (decoder-state-viterbi decoding-state)
                                                                previous previous-time)
                     ;;; The best probability as of yet:
                     with old of-type single-float = most-negative-single-float
                     ;; If the previous cell has a probability _lower_ than the best yet we can safely
                     ;; discard it from further processing. This saves a lot of calls to transition-probability() for big tagsets.
                     when (> prev-prob old)
                     ;; Get the bigram state-code for the previous tag.
                     do (multiple-value-bind (t1 t2)
                            (truncate previous n)
                          (declare (type fixnum t1 t2))
                          ;; Find the probability of transitioning from the previous tag into this tag and emitting the current word form
                          (let ((new (+ prev-prob
                                        emission
                                        (tri-cached-transition hmm t1 t2 current))))
                            (declare (type single-float new))
                            ;; If this was an improvement update the trellis and backpointer to reflect this
                            (when (> new old)
                              (setf old new) ; keep our `local' cache updated
                              (setf (aref (decoder-state-viterbi decoding-state)
                                          (encode-bigram t2 current) time) new)
                              (setf (aref (decoder-state-pointer decoding-state)
                                          (encode-bigram t2 current) time) previous)))))
              ;; Now swap the agendas and empty the next agenda
              finally
                (psetf (decoder-state-previous-possible decoding-state)
                       (decoder-state-next-possible decoding-state)
                       (decoder-state-next-possible decoding-state)
                       (decoder-state-previous-possible decoding-state))
                (setf (fill-pointer (decoder-state-next-possible decoding-state)) 0)
                ;; If we havent done anything at this time-step then we fail :-(
                (unless touch
                  (error "No tag generates current emission!"))))
    
    ;; Now we find the transition probability into the final state:
    (loop
        with end fixnum = (1- l)
        for code fixnum across (decoder-state-previous-possible decoding-state)
        do
          (multiple-value-bind (t1 t2)
              (truncate code n)
            (declare (type fixnum t1 t2))
            (let* ((prob (aref (decoder-state-viterbi decoding-state) code end)))
              (declare (type fixnum code)
                       (type single-float prob))
              (when (> prob (decoder-state-final decoding-state))
                (let ((new (+ prob
                              (tri-cached-transition hmm t1 t2 end-tag))))
                  (declare (type single-float new))
                  (when (> new (decoder-state-final decoding-state))
                    (setf (decoder-state-final decoding-state) new)
                    (setf (decoder-state-final-back decoding-state) code)))))))
    ;; And finally we follow the backpointers to get the actual best sequence of tags:
    (loop with time = (1- l)
        with last = (decoder-state-final-back decoding-state)
        with result = (list (code-to-bigram hmm last))
        for i fixnum from time downto 1
        for state = (aref (decoder-state-pointer decoding-state) last i)
        then (aref (decoder-state-pointer decoding-state) state i)
        do (push (code-to-bigram hmm state) result)
        finally (return
                    (mapcar #'second result)))))

(defun viterbi-bigram (decoder input &key (beam-width 13.80) &allow-other-keys)
  (declare (optimize (speed 3) (debug  1) (space 0)))
  (let* ((hmm (decoder-model decoder))
         (input (encode-input hmm input))
         (n (hmm-tag-cardinality hmm))
         (l (length input))
         (decoding-state (setup-bigram-decoding-state decoder input)))
    
    (declare (type fixnum n l))
    (loop
        with form = (first input)
        for state from 0 to (- n 1)
        with unk = (unknown-token-p hmm form)
        with unk-emi = (and unk (query-suffix-trie hmm (second form)))
        do (setf (aref (decoder-state-viterbi decoding-state) state 0)
                 (+ (bi-cached-transition hmm
                                          (token-to-code *start-tag* (hmm-tag-lexicon hmm))
                                          state)
                    (if unk
                      (aref unk-emi state)
                      (emission-probability hmm state form))))
          (setf (aref (decoder-state-pointer decoding-state)
                      state 0)
                0))
    (loop
        for form in (rest input)
        for time of-type fixnum from 1 to (- l 1)
        for unk = (unknown-token-p hmm form)
        for unk-emi = (and unk (query-suffix-trie hmm (second form)))
        with indices = (make-array n :fill-pointer t) ;; hmm-beam-array (hmm-beam-array hmm)
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
                    with old of-type single-float = (aref (decoder-state-viterbi decoding-state)
                                                          current time)
                    with emission of-type single-float = (if unk
                                                             (aref unk-emi current)
                                                           (emission-probability hmm current form))
                    for index fixnum from 0 to (1- (fill-pointer indices))
                    for previous = (aref indices index)
                    for prev-prob of-type single-float = (aref (decoder-state-viterbi decoding-state)
                                                               previous (1- time))
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
                          (setf (aref (decoder-state-viterbi decoding-state)
                                      current time)
                                new)
                          (setf (aref (decoder-state-pointer decoding-state)
                                      current time)
                                previous)))))
          (loop
              initially (setf (fill-pointer indices) 0)
              for current of-type fixnum from 0 to (- n 1)
              for prob of-type single-float = (the single-float
                                                   (aref (decoder-state-viterbi decoding-state)
                                                         current time))
              when (> prob best-hypothesis)
              do (vector-push current indices)))

    (loop
        with final = (token-to-code *end-tag* (hmm-tag-lexicon hmm))
        with time of-type fixnum = (- l 1)
        for previous of-type fixnum from 0 to (- n 1)
        for old of-type single-float = (aref (decoder-state-viterbi decoding-state)
                                             final time)
        for new of-type single-float = (+ (the single-float
                                               (aref (decoder-state-viterbi decoding-state)
                                                     previous time))
                                          (bi-cached-transition hmm previous final))
        when (> new old) do
          (setf (aref (decoder-state-viterbi decoding-state)
                      final time)
                new)
          (setf (aref (decoder-state-pointer decoding-state)
                      final time)
                previous))
    (if (null (aref (decoder-state-pointer decoding-state)
                    (token-to-code *end-tag* (hmm-tag-lexicon hmm)) (- l 1)))
        nil
      (loop
          with final = (token-to-code *end-tag* (hmm-tag-lexicon hmm))
          with time = (- l 1)
          with last  = (aref (decoder-state-pointer decoding-state)
                             final time)
          with result = (list (code-to-token last (hmm-tag-lexicon hmm)))
          for i of-type fixnum from time downto 1
          for state = (aref (decoder-state-pointer decoding-state)
                            last i)
          then (aref (decoder-state-pointer decoding-state)
                     state i)
          never (null state)
          do (push (code-to-token state (hmm-tag-lexicon hmm)) result)
          finally (return result)))))

(defstruct decoder
  (function nil)
  (model nil)
  (description nil))

(defun make-decoder-from-model (model description)
  (add-transition-table model description)
  
  (make-decoder :function (ecase (order description)
                            (1 #'viterbi-bigram)
                            (2 #'viterbi-trigram))
                :model model
                :description description))

(defun make-decoder-from-corpus (corpus description)
  (let ((model (train corpus)))
    (make-decoder-from-model model description)))

(defun decode (decoder sentence)
  (funcall (decoder-function decoder) decoder sentence))

(defun decode-start (decoder hmm input viterbi pointer &optional (constraints nil))
  (let ((n (hmm-tag-cardinality hmm)))
    (loop
     with form = (first input)
     for state from 0 to (- n 1)
     when (or (null constraints) (member state constraints)) 
     do (setf (aref viterbi state 0)
              (+ (bi-cached-transition hmm (token-to-code *start-tag* (hmm-tag-lexicon hmm)) state)
                 (emission-probability-slow decoder hmm state form)))
     and do (setf (aref pointer state 0) 0))))

(defun decode-form (decoder hmm time form viterbi pointer indices trigger
                            beam-width best-hypothesis &optional (constraints nil))
  (let ((n (hmm-tag-cardinality hmm)))
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
  (let ((n (hmm-tag-cardinality hmm)))
    (loop
     with final = (token-to-code *end-tag* (hmm-tag-lexicon hmm))
     with time of-type fixnum = (- l 1)
     for previous of-type fixnum from 0 to (- n 1)
     for old of-type single-float = (aref viterbi final time)
     for new of-type single-float = (+ (the single-float (aref viterbi previous time))
                                       (bi-cached-transition hmm previous final))
     when (> new old) do
     (setf (aref viterbi final time) new)
     (setf (aref pointer final time) previous))))

(defun backtrack-slow (hmm pointer l)
  (if (null (aref pointer (token-to-code *end-tag* (hmm-tag-lexicon hmm)) (- l 1)))
    nil
    (loop
     with final = (token-to-code *end-tag* (hmm-tag-lexicon hmm))
     with time = (- l 1)
     with last  = (aref pointer final time)
     with result = (list (code-to-token last (hmm-tag-lexicon hmm)))
     for i of-type fixnum from time downto 1
     for state = (aref pointer last i) then (aref pointer state i)
     never (null state)
     do (push (code-to-token state (hmm-tag-lexicon hmm)) result)
     finally (return result))))

(defun viterbi-bigram-slow (decoder input &key (beam-width 13.80) (constraints nil))
  ; (declare (optimize (speed 3) (debug  1) (space 0)))
  (let* ((hmm (decoder-model decoder))
         (input (encode-input hmm input))
         (n (hmm-tag-cardinality hmm))
         (l (length input))
         (constraints (loop for tags in constraints
                            collect (loop for tag in tags
                                          collect (token-to-code tag (hmm-tag-lexicon hmm)))))
         (viterbi (make-array (list n l)
                              :initial-element most-negative-single-float
                              :element-type 'single-float))
         (pointer (make-array (list n l) :initial-element nil)))
    ;;; Array initial element is not specified in standard, so we carefully
    ;;; specify what we want here. ACL and SBCL usually fills with nil and 0 respectively.
    (declare (type fixnum n l))
    (declare (type (simple-array single-float (* *)) viterbi)
             (type (simple-array t (* *)) pointer))

    (decode-start decoder hmm input viterbi pointer (first constraints))
    
    (loop
        for form in (rest input)
        for time of-type fixnum from 1 to (- l 1)

        with indices = (make-array n :fill-pointer t) ;; hmm-beam-array
    
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




