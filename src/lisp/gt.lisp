(in-package :mulm)

(defun coc-table (counts)
  (loop
      with table = (make-hash-table)
      for form being the hash-keys in counts
      for count = (gethash form counts)
      unless (eql :unk form)
      do (incf (gethash count table 0))
      finally 
	      (return table)))

(defparameter *cocs* nil)

(defun coc-list (coc-table)
  (let ((tmp))
    (maphash (lambda (k v) (push (list k v) tmp)) coc-table)
    (sort tmp  #'< :key #'car)))



(defun find-contig (coc-list)
  (loop
      for (count forms) = (first coc-list) then next
      for next in (rest coc-list)
      for index from 0
      while (= (1+ count) (first next))
      finally (return index)))


(defun adjust-count (r a b)
  (* (1+ r)
     (exp (- (+ (* (1+ r) a) b)
	     (+ (* r a) b)))))

(defun make-good-turing-estimate (counts total)
  (let* ((coc-table (coc-table counts))
	 (coc-list (coc-list coc-table))
	 (offset (min (find-contig coc-list) 25)))
    (unless (> 3 offset)
      (let*
	  ((params (fudge-smoothing (butlast coc-list (- (length coc-list) offset))))
	   (a (avg-list (mapcar #'first params)))
	   (b (avg-list (mapcar #'second params))))
	(loop
	    for key being the hash-keys in counts
	    for val = (gethash key counts)
	    when (< val offset)
	    do (setf (gethash key counts)
		 (adjust-count val a b))
	    finally (setf (gethash :unk counts)
		      (* (gethash :unk counts 0)
			 (adjust-count 0 a b))))))))
    
  
(defun fudge-smoothing (coc-vector)
  (loop
      with avgs
      for x1 = (first (first coc-vector)) then x2
      for y1 = (log (second (first coc-vector))) then (log  y2)
      for (x2 y2) in (rest coc-vector)
      for slope = (/ (float (- (log y2) y1))
		     (float (- x2 x1)))
      for offset = (- y2 (* x2 slope))
      do (push (list slope offset) avgs)	   
      finally (return avgs)))

(defun avg-list (list)
  (loop
      for x in list
      for i from 0
      sum x into accu
      finally (return (/ accu i))))
