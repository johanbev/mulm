(in-package :mulm)

;; simple trie data structure for storing unknown word suffix trees
;; currently not very efficient
(defstruct trie-node
  (value nil) (leaves nil))

(defun make-trie ()
  (make-trie-node))

(defun add-to-trie (trie nodes value)
  (cond ((null nodes)
         (when (not (null (trie-node-value trie)))
           (error "Value already assigned in trie"))
         (setf (trie-node-value trie) value))
        ((assoc (first nodes) (trie-node-leaves trie))
         (add-to-trie (second (assoc (first nodes) (trie-node-leaves trie)))
                      (rest nodes)
                      value))
        (t (let ((new-node (make-trie-node)))
             (push (list (first nodes) new-node)
                   (trie-node-leaves trie))
             (add-to-trie new-node (rest nodes) value)))))

(defun lookup-trie (trie nodes)
  (cond ((null nodes)
         (trie-node-value trie))
        ((assoc (first nodes) (trie-node-leaves trie))
         (lookup-trie (second (assoc (first nodes) (trie-node-leaves trie)))
                      (rest nodes)))
        (t nil)))
