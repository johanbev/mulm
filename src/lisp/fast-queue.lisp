(in-package :mulm)

(defstruct fast-queue
  size
  buffer
  (i-h 0)
  (cursor -1))

(defun mk-queue (size &key (element-type t))
  (make-fast-queue :size size :buffer (make-array size :element-type element-type)))

(defun fast-enqueue (queue item)
  (let* ((idx (rem (incf (fast-queue-cursor queue))
                   (fast-queue-size queue)))
         (head (if (<= (fast-queue-cursor queue) 2)
                   0
                 (rem (1+ idx)
                      (fast-queue-size queue)))))
    (setf (aref (fast-queue-buffer queue) idx) item)
    (setf (fast-queue-i-h queue) head))
  queue)

(defun fast-queue-to-list (queue)
  (loop
      repeat (min (fast-queue-size queue) (1+ (fast-queue-cursor queue)))
      for r-idx from (fast-queue-i-h queue)
      for idx = (rem r-idx (fast-queue-size queue))
      collect (aref (fast-queue-buffer queue) idx)))
    




