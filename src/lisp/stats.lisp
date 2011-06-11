(in-package :mulm)

;; this could go somewhere nicer
(defmacro get-or-add (key table add-form)
  (let ((tkey (gensym))
        (ttable (gensym)))
    `(let ((,tkey ,key)
           (,ttable ,table))
       (or (gethash ,tkey ,ttable) (setf (gethash ,tkey ,ttable) ,add-form)))))


(defvar *total-tokens* 0)

(defvar *word-counts*
    (make-hash-table))
(defvar *tags-of-word*
    (make-hash-table))

(defun reset-stats ()
  (setf *total-tokens* 0)
  (clrhash *word-counts*)
  (clrhash *tags-of-word*))

(defun register-observation (word tag)
  (incf *total-tokens*)
  (incf (gethash word *word-counts* 0))
  (incf (gethash tag
		 ;; ensure hashtable:
		 (get-or-add word *tags-of-word*
			     (make-hash-table))
		 0)))

(defun hash-table-sum (table)
  (let ((sum 0))
    (maphash (lambda (k v)
	       (declare (ignore k))
               (incf sum v))
             table)
    sum))

(defun hash-table-entropy (table)
  (loop
      with sum = (hash-table-sum table)
      for v being the hash-values in table
      summing (* -1 (/ v sum) (log (/ v sum) 2))))

;; Weighted average of entropies p(t|w)
(defun syncretisim-measure ()
  (loop
      for word being the hash-keys in *word-counts*
      for count = (gethash word *word-counts*)
      for p-w = (float (/ count *total-tokens*))
      for entropy = (hash-table-entropy (gethash word *tags-of-word*))
      summing (* p-w entropy)))
