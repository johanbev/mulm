(in-package :mulm)

(defun partition (list &optional (len 2))
  "Partitions the list into ordered sequences of len consecutive elements."
  (loop for i on list
        for j below (- (length list) (1- len))
        collect (subseq i 0 len)))

(defun true-p (form)
  (if form t))

(defun capitalized-p (string)
  (true-p (cl-ppcre:scan "^[A-Z]" string)))

(defun hash-table-sum (table)
  (let ((sum 0))
    (maphash (lambda (k v)
	       (declare (ignore k))
               (incf sum v))
             table)
    sum))

(defun hash-table-entropy (table)
  (loop
      with sum = (float (hash-table-sum table))
      for v being the hash-values in table
      when (or (= 1.0 sum) (= 0.0 v)) do (return 0.0)
      summing (* -1 (/ v sum) (log (/ v sum) 2))))

(defun weighted-average-of (table &key (key #'identity))
  (loop
      with total = (float (hash-table-sum table))
      for v being the hash-values in table
      summing (abs (* (/ v total) (funcall key (/ v total))))))

(defun renyi-entropy (table alpha)
  (if (= 1 alpha)
      (hash-table-entropy table)
    (* (/ 1 (- 1 alpha))
       (log
        (loop
            for v being the hash-values in table
            summing  (expt v alpha))
        2))))

(defun kl-divergence (p q)
  (loop
      for x being the hash-keys in p
      for P-x = (gethash x p)
      for Q-x = (gethash x q)
      summing (log (/ P-x Q-x))))

(defun renyi-divergence (p q alpha)
  (if (= 1 alpha)
      (kl-divergence p q)
  (* (/ 1.0 (- alpha 1.0))
     (log
      (loop
          for x being the hash-keys in p
          for P-x = (gethash x p)
          for Q-x = (gethash x q)
          summing (* (expt P-x alpha) (expt Q-x (- 1 alpha))))))))

(defmacro get-or-add (key table add-form)
  (let ((tkey (gensym))
        (ttable (gensym)))
    `(let ((,tkey ,key)
           (,ttable ,table))
       (or (gethash ,tkey ,ttable) (setf (gethash ,tkey ,ttable) ,add-form)))))

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

(defun list-to-plist (list)
  "Turns a list into a valid plist if possible, ie. items on even indexes are
   turned into keyword symbols.
   Returns the a new list that can be treated as a plist.
   Malformed input will most likely raise an error through intern()."
  (loop for elt in list
        for i from 1
        collect (if (oddp i)
                  (intern (string-upcase elt) :keyword)
                  elt)))
