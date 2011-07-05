(in-package :mulm)

(defstruct heap
  (backing-array (make-array 20000 :fill-pointer 1 :adjustable t) :type (array t (*)))
  next)

(defmacro parent (idx)
  `(the fixnum (truncate ,idx 2)))

(defmacro left-child (idx)
  `(the fixnum (* ,idx 2)))

(defmacro right-child (idx)
  `(the fixnum (1+ (the fixnum (* ,idx 2)))))

(defparameter *heap* nil)

(defun percolate-up (idx &key (heap *heap*))
  (declare (type fixnum idx))
  (if (= 1 idx)
      1
    (let ((val (aref (heap-backing-array heap) idx))
	  (parent (aref (heap-backing-array heap) (parent idx))))
      (if (< (the single-float (car parent))
	     (the single-float (car val)))
	  (progn
	    (psetf (aref (heap-backing-array heap) idx) parent
		   (aref (heap-backing-array heap) (parent idx)) val)
	    (percolate-up (parent idx)))
	idx))))

(defun percolate-down (idx &key (heap *heap*))
  (declare (type fixnum idx))
  (let ((val (aref (heap-backing-array heap) idx))
	(size (fill-pointer (heap-backing-array heap)))
	(left (left-child idx))
	(arr (heap-backing-array heap))
	(right (right-child idx)))
    (declare (type fixnum left right size))
    (declare (type (array t (*)) arr))
    (cond
     ((> left size) idx) ;; at end
     ;; can now have left or left and right child
     ((= left size) ;; had only left child
      (when (> (the single-float (car (aref arr left)))
	       (the single-float (car val)))
	(psetf 
	  (aref arr idx)
	  (aref arr left)
	  (aref arr left)
	  val))
      left)
     (t ;;both left and right
      (let ((left-val (car (aref arr left)))
	    (right-val (car (aref arr right))))
	(declare (type single-float left-val right-val))
	(if (> left-val right-val)
	    (if (> left-val (the single-float (car val)))
		(progn
		  (psetf 
		      (aref arr idx)
		    (aref arr left)
		    (aref arr left)
		    val)
		  (percolate-down left))
	      idx)
	  (if (>= right-val (the single-float (car val)))
	      (progn
		(psetf 
		    (aref arr idx)
		  (aref arr right)
		  (aref arr right)
		  val)
		(percolate-down right))
	    idx)))))))
      

(defun add-to-heap (pack &key (heap *heap*))
  (vector-push-extend pack (heap-backing-array heap))
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
  
