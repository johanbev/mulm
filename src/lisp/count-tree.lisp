(in-package :mulm)

(defstruct lm-tree-node
  weight
  (adds 0)
  (total 0)
  (emissions (make-hash-table :size 9))
  (children (make-hash-table :size 9)))

(defvar *lm-root* (make-lm-tree-node))

(defmethod print-object ((object lm-tree-node) stream)
  (with-slots (total children) object
    (format stream "#<LM-Node ~a decs, ~a total>" (hash-table-count children) total)))

(defun add-sequence (seq &optional (lm-root *lm-root*))
  (with-slots (total children) lm-root
    (incf total)
    (let ((child
           (get-or-add (first seq) children (make-lm-tree-node))))
      (if (rest seq)
          (add-sequence (rest seq) child)
        (incf (lm-tree-node-total child))))))

(defun sentence-to-n-grams (sentence n)
  (let ((buffer (make-instance 'lru-queue)))
    (initialize-instance buffer :size n)
    (loop
        for word in sentence
        for seq = (queue->list (enqueue buffer word))
        do (add-sequence seq)
        finally (mapl #'add-sequence (rest seq)))))

(defun build-model (sentences n)
  (loop 
      for sent in sentences
      do (sentence-to-n-grams 
          (append 
           (list "<s>")
           sent
           (list  "</s>" )) n)))
