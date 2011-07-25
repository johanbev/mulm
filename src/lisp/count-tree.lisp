(in-package :mulm)

(defstruct lm-tree-node
  weight
  (adds 0)
  (total 0)
  (emissions (make-hash-table :size 3))
  (children (make-hash-table :size 3)))

(defmethod print-object ((object lm-tree-node) stream)
  (with-slots (total children) object
    (format stream "#<LM-Node ~a decs, ~a total>" (hash-table-count children) total)))

(defun add-sequence (seq lm-root)
  (let ((children (lm-tree-node-children lm-root)))
    (incf (lm-tree-node-total lm-root))
    (let ((child
           (get-or-add (first seq) children (make-lm-tree-node))))
      (if (rest seq)
          (add-sequence (rest seq) child)
        (incf (lm-tree-node-total child))))))

(defun sentence-to-n-grams (sentence n lm-root)
  (let ((buffer (mk-queue n)))
    (loop
        for word in sentence
        for seq = (fast-queue-to-list (fast-enqueue buffer word))
        do (add-sequence seq lm-root)
        finally (mapl #'(lambda (x)
                          (add-sequence x lm-root))
                      (rest seq)))))

(defun build-model (sentences n lm-root)
  (loop 
      for sent in sentences
      do (sentence-to-n-grams sent n lm-root)))
