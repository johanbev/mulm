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
(defun syncretism-measure ()
  (loop
      for word being the hash-keys in *word-counts*
      for count = (gethash word *word-counts*)
      for p-w = (float (/ count *total-tokens*))
      for entropy = (hash-table-entropy (gethash word *tags-of-word*))
      summing (* p-w entropy)))


(defun confusion-matrix (blues golds &key (hmm *hmm*))
  (loop
      with tagset-size = (hmm-n hmm)
      with matrix = (make-array (list tagset-size tagset-size) :initial-element 0)
      for blue in blues
      for gold in golds
      do (incf (aref matrix (tag-to-code hmm blue) (tag-to-code hmm gold)))
      finally (return matrix)))


;; somewhat hacky, can require some human post-editing on the table
(defun print-confusion-matrix (matrix)
  (let ((length (first (array-dimensions matrix))))
    ;; print header
     (format t "   ~{~2,5T~a~}~%" (hmm-tags *hmm*))
    (loop    
	for i from 0 below length
	do (format t "~2,5T~a" (elt (hmm-tags *hmm*) i))
	   (loop 
	       for j from 0 below length
	       for point = (aref matrix i j)
	       do (format t "~3,5T~a" point))
	   (format t "~%"))))

(defun hash-table-diff (map1 map2)
  "Takes two hash-tables as arguments and returns nil if they have identical
   keys and values, t if they don't."
  (when (/= (hash-table-sum map1) (hash-table-sum map2))
    (return-from hash-table-diff t))
  (maphash #'(lambda (k val1)
               (let* ((nokey (gensym))
                      (val2 (gethash k map2 nokey)))
                 (if (or (equal val2 nokey) (not (equal val1 val2)))
                   (return-from hash-table-diff t))))
           map2)
  nil)

;; Prints out points where there is a difference between two models (trained
;; on the same material).
(defun model-diff (hmm1 hmm2)
  (let ((tags (hmm-tags hmm1)))
    (loop for tag1 in tags
          do (loop for tag2 in tags
                   do (let ((val1 (aref (hmm-transitions hmm1)
                                        (tag-to-code hmm1 tag1)
                                        (tag-to-code hmm1 tag2)))
                            (val2 (aref (hmm-transitions hmm2)
                                        (tag-to-code hmm2 tag1)
                                        (tag-to-code hmm2 tag2))))
                        (if (not (equal val1 val2))
                          (format t "~a ~a ~a ~a~%" tag1 tag2 val1 val2)))))
    (loop for tag in tags
          for map1 = (aref (hmm-emissions hmm1) (tag-to-code hmm1 tag))
          for map2 = (aref (hmm-emissions hmm2) (tag-to-code hmm2 tag))
          do (if (hash-table-diff map1 map2)
               (format t "~a~%" tag)))))
