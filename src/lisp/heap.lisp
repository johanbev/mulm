(in-package :mulm)

(defstruct heap
  (backing-array (make-array 100 :fill-pointer 1 :adjustable t))
  next)

(defmacro parent (idx)
  `(truncate ,idx 2))

(defmacro left-child (idx)
  `(* ,idx 2))

(defmacro right-child (idx)
  `(1+ (* ,idx 2)))

(defparameter *heap* nil)

(defun percolate-up (idx &key (heap *heap*))
  (if (= 1 idx)
      1
    (let ((val (aref (heap-backing-array heap) idx))
	  (parent (aref (heap-backing-array heap) (parent idx))))
      (if (< (car parent)
	     (car val))
	  (progn
	    (psetf (aref (heap-backing-array heap) idx) parent
		   (aref (heap-backing-array heap) (parent idx)) val)
	    (percolate-up (parent idx)))
	idx))))

(defun percolate-down (idx &key (heap *heap*))
  (let ((val (aref (heap-backing-array heap) idx))
	(size (fill-pointer (heap-backing-array heap)))
	(left (left-child idx))
	(right (right-child idx)))
    (cond
     ((> left size) idx) ;; at end
     ;; can now have left or left and right child
     ((= left size) ;; had only left child
      (when (> (car (aref (heap-backing-array heap) left))
	       (car val))
	(psetf 
	  (aref (heap-backing-array heap) idx)
	  (aref (heap-backing-array heap) left)
	  (aref (heap-backing-array heap) left)
	  val))
      left)
     (t ;;both left and right
      (let ((left-val (car (aref (heap-backing-array heap) left)))
	    (right-val (car (aref (heap-backing-array heap) right))))
	(if (> left-val right-val)
	    (if (> left-val (car val))
		(progn
		  (psetf 
		      (aref (heap-backing-array heap) idx)
		    (aref (heap-backing-array heap) left)
		    (aref (heap-backing-array heap) left)
		    val)
		  (percolate-down left))
	      idx)
	  (if (>= right-val (car val))
	      (progn
		(psetf 
		    (aref (heap-backing-array heap) idx)
		  (aref (heap-backing-array heap) right)
		  (aref (heap-backing-array heap) right)
		  val)
		(percolate-down right))
	    idx)))))))
      

(defun add-to-heap (priority element &key (heap *heap*))
  (vector-push-extend (cons priority element) (heap-backing-array heap))
  (percolate-up (1- (fill-pointer (heap-backing-array heap)))))

(defun pop-heap (&key (heap *heap*))
  (if (= 1 (fill-pointer (heap-backing-array heap)))
      nil
    (if (= (fill-pointer (heap-backing-array heap)) 2)
	(cdr (vector-pop (heap-backing-array heap)))
      (let ((ret (cdr (aref (heap-backing-array heap) 1))))
	(setf (aref (heap-backing-array heap) 1)
	  (vector-pop (heap-backing-array heap)))
	(percolate-down 1)
	ret))))

(defun adjust-priority (idx new-priority &key (heap *heap*))
  (setf (car (aref (heap-backing-array heap) idx)) new-priority)
  (percolate-up idx))
  
