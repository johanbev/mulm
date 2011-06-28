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