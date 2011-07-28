(in-package :mulm)

(defun partition (list &optional (len 2))
  "Partitions the list into ordered sequences of len consecutive elements."
  (loop for i on list
        for j below (- (length list) (1- len))
      collect (subseq i 0 len)))


(defun make-partition-stream (list &optional (len 2))
  (let ((pos list) (idx 0) (max (- (length list) (1- len))))
    (flet ((forward ()
             (setf pos (cdr pos))
             (incf idx)))
      (compile nil
      (lambda (&rest rest)
        (if (eq (first rest) :reset)
            (setf pos (second rest)
                  idx 0
                  max (- (length pos) (1- len)))
          (prog1 (if (< idx max)
                     (subseq pos 0 len)
                   nil)
            (forward))))))))

(defun collect-stream (stream)
  (loop
      for it = (funcall stream)
      while it
      collect it))
(defun make-lash-table (&rest rest)
  nil)

(defun getlash (key lash-table &optional default)
  (or
   (etypecase lash-table
     (list (cdr (assoc key lash-table)))
     (hash-table (gethash key lash-table)))
   default))


(defun maplash (fcn lash-table)
  (etypecase lash-table
    (list
     (loop for (key . value) in lash-table
         do (funcall fcn key value)))
    (hash-table
     (maphash fcn lash-table))))

(defun lash-table-count (lash-table)
  (etypecase lash-table
    (list (length lash-table))
    (hash-table (hash-table-count lash-table))))

(defmacro get-or-add-lash (key lash-table-place add-form)
  `(or (getlash ,key ,lash-table-place)
       (set-lash ,key ,add-form ,lash-table-place)))

(defmacro set-lash (key value lash-table-place)
  `(progn
     (let ((it ,value))
         (etypecase ,lash-table-place
           (list
            (block outer
              (loop
                  for cell in ,lash-table-place
                  for (lkey . val) = cell
                  when (eql ,key lkey) do
                    (setf (cdr cell) it)
                    (return-from outer))
              (setf ,lash-table-place                     
                (push (cons ,key it) ,lash-table-place))))
           (hash-table
            (setf (gethash ,key ,lash-table-place) it)))
       (when (and (listp ,lash-table-place) (> (length ,lash-table-place) 24))
         (let ((tbl (make-hash-table :size 17)))
           (mapcar (lambda (x) (setf (gethash (car x) tbl) (cdr x)))
                   ,lash-table-place)
           (setf ,lash-table-place tbl)))
       it)))



        

(defun true-p (form)
  (if form t))

(defun capitalized-p (string)
  (true-p (cl-ppcre:scan "^[A-Z]" string)))

(defun hash-table-sum (table)
  (let ((sum 0))
    (declare (type fixnum sum))
    (maphash (lambda (k v)
	       (declare (ignore k))
               (incf sum (the fixnum v)))
             table)
    sum))

(defun lash-table-sum (table)
  (if table
      (if (and (listp table) (null (cdr table)))
          (cdar table)
        (let ((sum 0))
          (declare (fixnum sum))
          (maplash (lambda (k v)
                     (declare (ignore k))
                     (incf sum (the fixnum v)))
                   table)
          sum))
    0))

(defun lash-table-entropy (table)
  (if (and (listp table) (null (cdr table)))
      0.0
  (if table
    (let* ((sum (float (lash-table-sum table)))
           (accu 0.0))
      (declare (type single-float accu sum))
      (loop
          for v in (lash-values table)
          for div of-type single-float = (/ v sum)
          do
            (incf accu (* -1.0 (the single-float div) (log div 2))))            
      accu))))

(defun lash-keys (table)
  (let (accu)
    (maplash (lambda (k v)
               (declare (ignore v))
               (push k accu))
             table)
    accu))

(defun lash-values (table)
  (let (accu)
    (maplash (lambda (v k)
               (declare (ignore v))
               (push k accu))
             table)
    accu))

(defun hash-table-entropy (table)
  (loop
      with sum of-type single-float = (float (hash-table-sum table))
      for v being the hash-values in table
      for div single-float = (/ (the single-float (float v)) sum)
      summing (* -1.0 div (log div 2))))

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
