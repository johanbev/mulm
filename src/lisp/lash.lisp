(in-package :mulm)

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
