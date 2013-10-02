(in-package :mulm)

; Extracts feature representation at pos in context.
; Returns function taking observation context, position (zero indexed) and tags at position -1 and -2.
; This function returns nil if the feature representation can not be extracted:
; - Position is outside of context.
; - Feature requests context outside of range.
; - Tag -1 or -2 is not available.
(defun make-feature-representation (id &key (word t) (extractor #'identity) (offset 0) (tag-1 nil) (tag-2 nil))
  (list id
        (lambda (context pos prev-1 prev-2)
          (if (or (and tag-1 (null prev-1))
                  (and tag-2 (null prev-2))
                  (>= pos (length context))
                  (< (+ pos offset) 0))
            nil
            (let ((features nil))
              (when (and word (>= (+ offset pos) 0) (< (+ offset pos) (length context)))
                (push (intern (funcall extractor
                                       (elt context (+ offset pos)))
                              :keyword)
                      features))
              (when (and tag-1 prev-1)
                (push (intern prev-1 :keyword)
                      features))
              (when (and tag-2 prev-2)
                (push (intern prev-2 :keyword)
                      features))
              (if features
                (cons id (nreverse features))))))))

(defun make-suffix-extractor (&optional (n 3))
  (lambda (str)
    (subseq str (max (- (length str) n) 0))))

(defun make-prefix-extractor (&optional (n 1))
  (lambda (str)
    (subseq str 0 (min n (length str)))))

(defparameter *default-factor-features*
  (list
   (make-feature-representation :suffix
                                :extractor (make-suffix-extractor))
   (make-feature-representation :prefix
                                :extractor (make-prefix-extractor))
   (make-feature-representation :tag-1 :word nil :tag-1 t)
   (make-feature-representation :tag-2 :word nil :tag-2 t)
   (make-feature-representation :tag-1-tag-2 :word nil :tag-1 t :tag-2 t)
   (make-feature-representation :word)
   (make-feature-representation :word-tag-1 :tag-1 t)
   (make-feature-representation :word-1 :offset -1)
   (make-feature-representation :suffix-1
                                :offset -1
                                :extractor (make-suffix-extractor))
   (make-feature-representation :word-2 :offset -2)
   (make-feature-representation :word+1 :offset 1)
   (make-feature-representation :suffix+1
                                :offset +1
                                :extractor (make-suffix-extractor))
   (make-feature-representation :word+2 :offset +2)))

(defun add-feature-count (id val counts)
  (let ((counts-for-id (or (gethash id counts)
                           (setf (gethash id counts)
                                 (make-hash-table :test #'equal)))))
    (incf (gethash val counts-for-id 0))))

(defun get-feature-count (id val counts)
  (let ((counts-for-id (gethash id counts)))
    (if counts-for-id
      (gethash val counts-for-id))))

(defun features-from-context (features context pos prev-1 prev-2)
  (loop for (nil feat-ex) in features
        for feature = (funcall feat-ex context pos prev-1 prev-2)
        when feature
        collect feature))

(defun count-features (sentence features counts)
  (let ((context (mapcar #'first sentence))
        (tags (mapcar #'second sentence)))
    (loop for pos from 0 below (length sentence)
          do (loop for feature in (features-from-context features context pos
                                                         (if (>= pos 1) (elt tags (- pos 1)) "<s>")
                                                         (if (>= pos 2) (elt tags (- pos 2)) "<s>"))
                   for feat-id = (first feature)
                   do (add-feature-count feat-id feature counts))))
  counts)

(defclass feature-factor ()
  ((feature-index-map :initform (make-hash-table :test #'equal))
   (index-feature-map :initform nil)
   (target-index-map :initform (make-hash-table :test #'equal))
   (index-target-map :initform nil)
   (features :initform *default-factor-features*
             :initarg :features)
   (cutoff :initform 5
           :initarg :cutoff)
   (p :initform 0)
   (c :initform 0)
   (w :initform nil)))


(defmethod initialize-instance :after ((factor feature-factor) &key (corpus nil))
  (with-slots (feature-index-map index-feature-map target-index-map index-target-map features cutoff p w c)
      factor
    (let ((counts (make-hash-table)))

      ; collect feature counts and tag list
      (loop for sent in corpus
            do (count-features sent features counts)
            do (loop for (nil tag) in sent
                     for y = (intern tag :keyword)
                     unless (gethash y target-index-map)
                     do (progn  (setf (gethash y target-index-map) c)
                          (incf c))))

      ; construct feature maps
      (loop for count-map being the hash-values in counts
            do (loop for feature being the hash-keys in count-map
                     for count being the hash-values in count-map
                     when (>= count cutoff)
                     do (progn
                          (unless (gethash feature feature-index-map)
                            (setf (gethash feature feature-index-map) p)
                            (incf p)))))

      (setf index-feature-map (make-array p))
      (loop for feature being the hash-keys in feature-index-map
            for index being the hash-values in feature-index-map
            do (setf (aref index-feature-map index) feature))

      ; reverse tag map
      (setf index-target-map (make-array c))
      (loop for y being the hash-keys in target-index-map
            for index being the hash-values in target-index-map
            do (setf (aref index-target-map index) y))

      ; initialize perceptron weights
      (setf w (make-array (* c p) :element-type 'float :initial-element 0.0))

      factor)))

(defgeneric feature-vector (factor sent pos prev-1 prev-2 &key))

(defgeneric vector-activation (factor vector &key))

(defmethod feature-vector ((factor feature-factor) sent pos prev-1 prev-2 &key)
  (with-slots (features feature-index-map) factor
    (sort
     (loop for feature in (features-from-context features sent pos prev-1 prev-2)
           collect (gethash feature feature-index-map))
     #'<)))

(defmethod vector-activation ((factor feature-factor) vector &key)
  (with-slots (c p w) factor
    (loop for i from 0 below c
          for class-weights = (make-array p :displaced-to w :displaced-index-offset (* i p))
          collect (loop for index in vector
                        sum (aref class-weights index)))))
