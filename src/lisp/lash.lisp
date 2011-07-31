(in-package :mulm)

(defstruct lash
  table)

(defun getlash (key lash-table &optional default)
  (let ((lash-table (lash-table lash-table)))
    (or
     (etypecase lash-table
       (list (cdr (assoc key lash-table)))
       (hash-table (gethash key lash-table)))
     default)))

(defun set-lash (key lash-table &rest args)
  (progn
    (let ((table (lash-table lash-table))
          (it (case (length args)
                (1 (first args))
                (2 (second args)))))
         (etypecase table
           (list
            (block outer
              (loop
                  for cell in table
                  when (eq key (car cell)) do
                    (setf (cdr cell) it)
                    (return-from outer))
              (setf (lash-table lash-table)
                (push (cons key it) table))))
           (hash-table
            (setf (gethash key table) it)))
         (when (and (listp table) (> (length (the cons table)) 24))
           (let ((tbl (make-hash-table :size 24)))
             (mapcar (lambda (x) (setf (gethash (car x) tbl) (cdr x)))
                     table)
             (setf (lash-table lash-table) tbl)))
         it)))

(defmacro with-lash (lash &body body)
  `(let ((lash (lash-table ,lash)))
     ,@body))

(defmacro get-or-add-lash (key lash add-form)
  `(or (getlash ,key ,lash)
       (set-lash ,key ,lash ,add-form)))

(defsetf getlash set-lash)

(defun maplash (fcn lash)
  (when (and lash (lash-table lash))
    (with-lash lash
      (etypecase lash
        (list
         (loop for (key . value) in lash
             do (funcall fcn key value)))
        (hash-table
         (maphash fcn lash))))))
  
(defun lash-table-count (lash)
  (with-lash lash
    (etypecase lash
      (list (length lash))
      (hash-table (hash-table-count lash)))))

(defun lash-table-sum (lash-table)
  (with-lash lash-table
    (if lash
        (if (and (listp lash) (null (cdr lash)))
            (cdar lash)
          (let ((sum 0))
            (declare (fixnum sum))
            (maplash (lambda (k v)
                       (declare (ignore k))
                       (incf sum (the fixnum v)))
                     lash-table)
            sum))
      0)))

(defun lash-table-entropy (lash-table)
  (with-lash lash-table
    (etypecase lash
      (list
       (let* ((sum (float (lash-table-sum lash-table)))
              (accu 0.0))
         (declare (type single-float accu sum))
         (loop
             for v in (lash-values lash-table)
             for div of-type single-float = (/ v sum)
                do
               (incf accu (* -1.0 (the single-float div) (log div 2))))            
         accu))
      (hash-table
       (hash-table-entropy lash)))))

(defun lash-keys (lash)
    (let (accu)
      (maplash (lambda (k v)
                 (declare (ignore v))
                 (push k accu))
               lash)
      accu))

(defun lash-values (lash)
  (let (accu)
    (maplash (lambda (v k)
               (declare (ignore v))
               (push k accu))
             lash)
    accu))
