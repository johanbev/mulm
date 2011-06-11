(in-package :mulm)

(defstruct (symbol-table 
            (:constructor make-symbol-table 
                          (&key (test #'equal)
                                (forward (make-hash-table :test test))
                                (backward (make-array 512))
                                (size 512)
                                (count 0))))
  forward backward size count)

(defvar *symbol-table*
    (make-symbol-table))

(defmethod print-object ((object symbol-table) stream)
  (let ((n (hash-table-count (symbol-table-forward object))))
    (format 
     stream 
     "#<Symbol Table (~d forward~p, ~d backward~p of ~s)>"
     n n (symbol-table-count object) (symbol-table-count object)
     (symbol-table-size object))))

(defun symbol-to-code (symbol
                       &optional (table *symbol-table*)
                       &key rop)
  (or
   (gethash symbol (symbol-table-forward table))
   (unless rop
     (let* ((i (symbol-table-count table)))
       (setf (gethash symbol (symbol-table-forward table)) i)
       (when (>= i (symbol-table-size table))
         (setf (symbol-table-size table) (* 2 (symbol-table-size table)))
         (setf (symbol-table-backward table)
           (adjust-array 
            (symbol-table-backward table) (symbol-table-size table))))
       (setf (aref (symbol-table-backward table) i) symbol)
       (incf (symbol-table-count table))
       i))))


(defun code-to-symbol (code &optional (table *symbol-table*))
  (when (< code (symbol-table-count table))
    (aref (symbol-table-backward table) code)))

(defun set-symbol-and-code (symbol code &optional (table *symbol-table*))
  (setf (gethash symbol (symbol-table-forward table)) code)
  (when (>= code (symbol-table-size table))
    (setf (symbol-table-size table) (* 2 (symbol-table-size table)))
    (setf (symbol-table-backward table)
      (adjust-array (symbol-table-backward table) (symbol-table-size table))))
  (setf (aref (symbol-table-backward table) code) symbol)
  (incf (symbol-table-count table)))

