(in-package :mulm)

(defun emission-extractor (sentence pos)
  (loop for token in (elt sentence pos)
        collect (intern token :keyword)))

(defun bigram-extractor (sentence pos)
  (let ((tag (second (elt sentence pos)))
        (tag-1 (if (= pos 0)
                 "<s>"
                 (second (elt sentence (1- pos))))))
    (list (intern tag-1 :keyword)
          (intern tag :keyword))))

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
