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

(defmacro get-or-add-lash (key lash add-form)
  `(or (getlash ,key ,lash)
       (set-lash ,key ,lash ,add-form)))

(defsetf getlash set-lash)

(defun maplash (fcn lash)
  (let ((lash-internal (lash-table lash)))
    (etypecase lash-internal
      (list
       (loop for (key . value) in lash-internal
             do (funcall fcn key value)))
      (hash-table
       (maphash fcn lash-internal)))))
  
(defun lash-table-count (lash)
  (let ((lash-internal (lash-table lash)))
    (etypecase lash-internal
      (list (length lash-internal))
      (hash-table (hash-table-count lash-internal)))))

(defun lash-table-sum (lash &key (key #'identity))
  (let ((lash-internal (lash-table lash)))
    (if lash-internal
        (if (and (listp lash-internal) (null (cdr lash-internal)))
            (funcall key (cdar lash-internal))
          (let ((sum 0))
            (maplash (lambda (k v)
                       (declare (ignore k))
                       (incf sum (funcall key v)))
                     lash)
            sum))
      0)))

(defun lash-table-entropy (lash &key (key #'identity))
  (let ((lash-internal (lash-table lash)))
    (etypecase lash-internal
      (list
       (let* ((sum (float (lash-table-sum lash :key key)))
              (accu 0.0))
         (declare (type single-float accu sum))
         (loop
             for raw in (lash-values lash)
             for v = (funcall key raw)
             for div of-type single-float = (if (= sum 0.0) ;; division by zero guard
                                                0.0
                                              (/ v sum))
             do
               (incf accu (* -1.0 (the single-float div)
                             (if (= div 0.0) ;; log of zero guard
                                 0
                               (log div 2.0)))))            
         accu))
      (hash-table
       (hash-table-entropy lash-internal :key key)))))

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
