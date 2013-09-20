(in-package :mulm)

; Extracts feature representation at pos in context.
; Returns function taking observation context, position (zero indexed) and tags at position -1 and -2.
; This function returns nil if the feature representation can not be extracted:
; - Position is outside of context.
; - Feature requests context outside of range.
; - Tag -1 or -2 is not available.
(defun make-feature-representation (id &key (word t) (extractor #'identity) (offset 0) (tag-1 nil) (tag-2 nil))
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
          (cons id (nreverse features)))))))

(defun make-suffix-extractor (&optional (n 3))
  (lambda (str)
    (subseq str (max (- (length str) n) 0))))

(defun make-prefix-extractor (&optional (n 1))
  (lambda (str)
    (subseq str 0 (min n (length str)))))

(defparameter *default-factor-features*
  `((:suffix ,(make-feature-representation :suffix
                                           :extractor (make-suffix-extractor)))
    (:prefix ,(make-feature-representation :prefix
                                           :extractor (make-prefix-extractor)))
    (:tag-1 ,(make-feature-representation :tag-1 :word nil :tag-1 t))
    (:tag-2 ,(make-feature-representation :tag-2 :word nil :tag-2 t))
    (:tag-1-tag-2 ,(make-feature-representation :tag-1-tag-2 :word nil :tag-1 t :tag-2 t))
    (:word ,(make-feature-representation :word))
    (:word-tag-1 ,(make-feature-representation :word-tag-1 :tag-1 t))
    (:word-1 ,(make-feature-representation :word-1 :offset -1))
    (:suffix-1 ,(make-feature-representation :suffix-1
                                             :offset -1
                                             :extractor (make-suffix-extractor)))
    (:word-2 ,(make-feature-representation :word-2 :offset -2))
    (:word+1 ,(make-feature-representation :word+1 :offset 1))
    (:suffix+1 ,(make-feature-representation :suffix+1
                                             :offset +1
                                             :extractor (make-suffix-extractor)))
    (:word+2 ,(make-feature-representation :word+2 :offset +2))))

(defun add-feature-count (id val counts)
  (let ((counts-for-id (or (gethash id counts)
                           (setf (gethash id counts)
                                 (make-hash-table :test #'equal)))))
    (incf (gethash val counts-for-id 0))))

(defun get-feature-count (id val counts)
  (let ((counts-for-id (gethash id counts)))
    (if counts-for-id
      (gethash val counts-for-id))))

(defun count-features (sentence features counts)
  (let ((context (mapcar #'first sentence))
        (tags (mapcar #'second sentence)))
    (loop for (feat-id repr) in features
          do (loop for pos from 0 below (length sentence)
                   for feat = (funcall repr context pos
                                       (if (>= pos 1) (elt tags (- pos 1)) "<s>")
                                       (if (>= pos 2) (elt tags (- pos 2)) "<s>"))
                   when feat
                   do (add-feature-count feat-id feat counts))))
  counts)
