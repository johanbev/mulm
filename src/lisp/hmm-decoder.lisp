(in-package :mulm)

(defvar *bigrams* nil)

(defun viterbi-trigram (hmm input &key (bigrams *bigrams*) &allow-other-keys)
  (declare (optimize (speed 3) (debug  1) (space 0)))
  (let* ((n (hmm-n hmm))
         (nn (* n n))
         (l (length input))
         (viterbi (make-array (list nn l) :initial-element most-negative-single-float :element-type 'single-float))
         ;; TODO use more memory friendly type for backpointer table
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
    ;; LW6 can't handle enormous allocations on the stack
    #+allegro
    (declare (dynamic-extent viterbi pointer))
    (loop 
        with form = (first input)
        with unk = (eql :unk (gethash form *known-codes* :unk))
        with unk-emi = (and unk (query-suffix-trie hmm form))
        for tag fixnum from 0 to (- n 1)
        for state fixnum = (+ (the fixnum (* start-tag n)) tag)
        do (setf (aref viterbi state 0)
             (+ (the single-float (if bigrams
                                      (aref (the (simple-array single-float (* *)) bigrams) start-tag tag)
                                    (transition-probability hmm start-tag tag
                                                            :order 1 :smoothing :deleted-interpolation)))
                (the single-float
                  (if unk
                      (aref unk-emi tag)
                    (emission-probability hmm tag form)))))
        do (setf (aref pointer state 0) 0))
    (loop 
        for form in (rest input)
        for time fixnum from 1 to (- l 1)
        for previous-time fixnum = (1- time)
        for unk = (eql :unk (gethash form *known-codes* :unk))
        for unk-emi = (and unk (query-suffix-trie hmm form))
        do (loop
               with touch = nil
               for current fixnum from 0 to (- n 1)
               for emission of-type single-float = (if unk
                                                       (aref unk-emi current)
                                                     (emission-probability hmm current form))
               when (or unk (> emission -19.0))                                           
               do (setf touch t)
                  (loop
                    ;;; the loop of death, we really don't want to go here if we can spare it
                      for previous fixnum  from 0 below nn
                      for prev-prob of-type single-float = (aref viterbi previous previous-time)
                      with old of-type single-float = (aref viterbi current time)
                      when (> prev-prob old)
                      do (multiple-value-bind (t1 t2)
                             (truncate previous n)
                           (declare (type fixnum t1 t2))
                           (let ((new (+ prev-prob
                                         emission
                                         (tri-cached-transition hmm t1 t2 current))))
                             (declare (type single-float new))
                             (when (> new old)
                               (setf old new)
                               (setf (aref viterbi (the fixnum (+ (the fixnum (* t2 n)) current)) time) new)
                               (setf (aref pointer (the fixnum (+ (the fixnum (* t2 n)) current)) time) previous))))
                  (unless touch
                    ;; rescue loop, shoudn't come here normally.                    
                    (format t "~% To the rescue!")
                    (loop                      
                        for current fixnum from 0 to (- n 1)
                        for emission of-type single-float = (if unk
                                                                (aref unk-emi current)
                                                              (emission-probability hmm current form))
                        do
                          (loop
                              for previous from 0 below nn
                              for prev-prob of-type single-float = (aref viterbi previous previous-time)
                              with old of-type single-float = (aref viterbi current time)
                              do (multiple-value-bind (t1 t2)
                                     (truncate previous n)
                                   (declare (type fixnum t1 t2))
                                   (let ((new (+ prev-prob
                                                 emission
                                                 (tri-cached-transition hmm t1 t2 current))))
                                     (declare (type single-float new))
                                     (when (> new old)
                                       (setf old new)
                                       (setf (aref viterbi (the fixnum (+ (the fixnum (* t2 n)) current)) time) new)
                                       (setf (aref pointer (the fixnum (+ (the fixnum (* t2 n)) current)) time) previous))))))))))
    (loop
        with time fixnum = (1- l)
        for previous fixnum from 0 below nn
        do
          (multiple-value-bind (t1 t2)
                             (truncate previous n)
            (declare (type fixnum t1 t2))
            (let ((new (+ (aref viterbi previous time)
                          (tri-cached-transition hmm t1 t2 end-tag))))
              (declare (type single-float new))
              (when (> new final)
                (setf final new)
                (setf final-back previous)))))
    (loop with time = (1- l)
        with last = final-back
        with result = (list (code-to-bigram hmm last))
        for i fixnum from time downto 1
        for state fixnum  = (aref pointer last i) then (aref pointer state i)
        do (push (code-to-bigram hmm state) result)
        finally (return
                    (mapcar #'second result)))))

;;; hvor conser denne?
(defun viterbi-bigram (hmm input &key (beam-width 13.80) &allow-other-keys)
  (declare (optimize (speed 3) (debug  1) (space 0)))
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

(defstruct viterbi-decoder
  function
  model
  (caches (make-hash-table)))

(defun make-decoder (training-corpus)
  (init-decoder
   (make-viterbi-decoder :function #'viterbi-bigram-slow
                         :model (train training-corpus))))

(defun init-decoder (decoder)
  (setf (gethash :unknown-word (viterbi-decoder-caches decoder))
        (make-hash-table))
  decoder)

(defun viterbi-bigram-slow (hmm input &key (beam-width 13.80) (decoder nil))
  (declare (ignore hmm))
  ; (declare (optimize (speed 3) (debug  1) (space 0)))
  (let* ((hmm (viterbi-decoder-model decoder))
         (n (hmm-n hmm))
         (l (length input))
         (viterbi (make-array (list n l)
                              :initial-element most-negative-single-float
                              :element-type 'single-float))
         (pointer (make-array (list n l) :initial-element nil)))
    ;;; Array initial element is not specified in standard, so we carefully
    ;;; specify what we want here. ACL and SBCL usually fills with nil and 0 respectively.
    (declare (type fixnum n l))
    (declare (type (simple-array single-float (* *)) viterbi)
             (type (simple-array t (* *)) pointer))
    (init-decoder decoder)
    (loop
        with form of-type fixnum = (first input)
        for state of-type fixnum from 0 to (- n 1)
        do (setf (aref viterbi state 0)
                 (+ (bi-cached-transition hmm (tag-to-code hmm "<s>") state)
                    (emission-probability-slow decoder hmm state form)))
        do (setf (aref pointer state 0) 0))
    (loop
        for form of-type fixnum in (rest input)
        for time of-type fixnum from 1 to (- l 1)
        with indices = (hmm-beam-array hmm)
        initially (setf (fill-pointer indices) 0)
                  (loop 
                      for x below n
                      do (vector-push x indices))
        for best-hypothesis of-type single-float = most-negative-single-float
        for trigger of-type single-float = most-negative-single-float
        do (loop
            for current of-type fixnum from 0 to (- n 1)
            do (loop
                with old of-type single-float = (aref viterbi current time)
                with emission of-type single-float = (emission-probability-slow decoder hmm current form)
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