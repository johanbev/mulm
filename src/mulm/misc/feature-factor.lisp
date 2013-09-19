(in-package :mulm)

(defun make-word-feature-extractor (&key (offset 0) (processor #'identity))
  (lambda (sentence pos)
    (let ((offset-pos (+ pos offset)))
      (if (or (>= offset-pos (length sentence))
              (< offset-pos 0))
        nil
        (intern (funcall processor (first (elt sentence offset-pos)))
                :keyword)))))

(defun make-ngram-feature-extractor (&key (n 2))
  (lambda (sentence pos)
    (if (or (< pos 0) (>= pos (length sentence)))
      nil
      (let* ((start (abs (min (- pos (1- n)) 0)))
             (start-tags (loop repeat start
                               collect :|<s>|))
             (tags (loop for tag in (subseq (mapcar #'second sentence) (max (- pos (1- n)) 0) (1+ pos))
                         collect (intern tag :keyword))))
        (append start-tags tags)))))

(defparameter *default-factor-features*
  `((:emission ,(make-word-feature-extractor))
    (:bigram ,(make-ngram-feature-extractor))))

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
  (loop for (feat-id extractor) in features
        do (loop for pos from 0 below (length sentence)
                 do (add-feature-count feat-id (funcall extractor sentence pos) counts)))
  counts)
