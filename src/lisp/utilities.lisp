(in-package :mulm)

(defun partition (list &optional (len 2))
  "Partitions the list into ordered sequences of len consecutive elements."
  (loop for i on list
        for j below (- (length list) (1- len))
      collect (subseq i 0 len)))

(defun make-partition-stream (list &optional (len 2))
  "Create a generator yielding the partition of list into ordered sequences of
   len consecutive elements. The generator is reusable by funcalling it with
   (:reset new-list) as arguments"
  (let ((pos list) (idx 0) (max (- (length list) (1- len))))
    (flet ((forward ()
             (setf pos (cdr pos))
             (incf idx)))
      (lambda (&rest rest)
        (if (eq (first rest) :reset)
          (setf pos (second rest)
                idx 0
                max (- (length pos) (1- len)))
          (prog1 (if (< idx max)
                   (subseq pos 0 len)
                   nil)
            (forward)))))))

(defun collect-stream (stream)
  "Collects the values yielded by the generator until nil is yielded"
  (loop
      for it = (funcall stream)
      while it
      collect it))

(defun true-p (form)
  (if form t))

(defun capitalized-p (string)
  (true-p (cl-ppcre:scan "^[A-Z]" string)))

(defun hash-table-sum (table &key (key #'identity))
  "Computes the sum of all the values in a hash-table"
  (loop
      for raw being the hash-values in table
      for val = (funcall key raw)
      summing val))

(defun hash-table-entropy (table &key sum (key #'identity))
  "Computes the entropy of all the unnormalized values in a hash-table,
   ie. the values are counts C(X=x)"   
  (loop
      with sum of-type single-float = (float (or sum (hash-table-sum table :key key)))
      for raw of-type number being the hash-values in table
      for v of-type number = (funcall key raw)
      for div of-type single-float = (/ v sum)
      when (= 1.0 sum) do (return 0.0)
      unless (zerop div)
      summing (the single-float (* -1.0 div (log div 2.0)))))

(defun weighted-average-of (table &key (key #'identity))
  "Computes the weighted-average of calling key using the counts in the
   table as weights"
  (loop
      with total of-type single-float = (float (hash-table-sum table))
      for v being the hash-values in table
      for div of-type single-float  = (/ v total)
      summing (abs (* div (funcall key div)))))

(defun renyi-entropy (table alpha &optional sum)
  "Computes the renyi-entropy of all the unnormalized values in a hash-table,
   ie. the values are counts C(X=x)"
  (if (= 1 alpha)
      (hash-table-entropy table :sum sum)
    (* (/ 1 (- 1 alpha))
       (log
        (loop
            with sum of-type single-float = (float (or sum (hash-table-sum table)))
            for v being the hash-values in table
            for div of-type single-float = (/ v sum)
            summing  (expt div alpha))
        2))))

(defun kl-divergence (p q)
  "Computes the Kullback-Leibler divergence DKL(P || Q),
   p and q are hash-tables with counts C(X=x)"
  (loop
      with p-sum = (float (hash-table-sum p))
      with q-sum = (float (hash-table-sum q))
      for x being the hash-keys in p
      for P-x = (/ (gethash x p) p-sum)
      for Q-x = (/ (gethash x q) q-sum)
      summing (log (/ P-x Q-x))))

(defun renyi-divergence (p q alpha)
  "Computes the Renyi-divergence DR(P || Q),
   p and q are hash-tables with counts C(X=x)"
  (if (= 1 alpha)
      (kl-divergence p q)
  (* (/ 1.0 (- alpha 1.0))
     (log
      (loop
          with p-sum = (float (hash-table-sum p))
          with q-sum = (float (hash-table-sum q))
          for x being the hash-keys in p
          for P-x = (/ (gethash x p) p-sum)
          for Q-x = (/ (gethash x q) q-sum)
          summing (* (expt P-x alpha) (expt Q-x (- 1 alpha))))))))

(defmacro get-or-add (key table add-form)
  "Gets `key' from hash-table `table'. If `key' is not in the table it will be added first
   by evaluating `add-form'"
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

(defun get-command-line ()
  (or 
   #+sbcl sb-ext:*posix-argv*
   #+lispworks system:*line-arguments-list*
   #+allegro (sys:command-line-arguments)
   nil))

(defun get-command-line-args ()
  (or
   ;; SBCL and LW includes command path
   #+(or sbcl lispworks) (rest (get-command-line))
   #+(allegro) (rest (sys:command-line-arguments))
   nil))

(defun make-keyword (str)
  (if str (intern (string-upcase str) :keyword)))

(defun quit (&key (status 0))
  #+lispworks
  (lispworks:quit :status status)
  #+sbcl
  (sb-ext:quit :unix-status status)
  #+allegro
  (excl:exit status))
