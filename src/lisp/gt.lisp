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
      for (count) = (first coc-list) then next
      for next in (rest coc-list)
      for index from 0
      while (= (1+ count) (first next))
      finally (return index)))


(defun adjust-count (r a b)
  (* (1+ r)
     (exp (- (+ (* a (log (1+ r))) b)
	     (+ (* a (log r)) b)))))

(defun adjust-zero (a)
  (exp a))

(defvar *verbose-gt* nil)

(defun make-good-turing-estimate (counts total &optional tag)
  (declare (ignorable total))
  (cond 
   ((< (hash-table-count counts) 3) counts)
   (t
    (let* ((coc-table (coc-table counts))
           (coc-list (coc-list coc-table))
           (offset (min (find-contig coc-list) 6)))
      (unless (> 2 offset)
        (let*
            ((params (fudge-smoothing (butlast coc-list (- (length coc-list) offset))))
             (a (avg-list (mapcar #'first params)))
             (b (avg-list (mapcar #'second params))))
          (unless (> a -1.0)
            (loop
                for key being the hash-keys in counts
                for val = (gethash key counts)
                with ones = (second (first coc-list))
                when (< val offset)
                do (setf (gethash key counts)
                     (adjust-count val a b))
                finally (when (and tag 
                                   *verbose-gt*)
                          (format t 
                                  "~&make-good-turing-estimate():~% Estimating `~a' with ~a emissions and ~a types.
 A:~,2f [~,2f]  B:~,2f [~,2f] Est. Unknown-count: ~,2f Est. Unknowns: ~,2f
 New counts: ~{~{~a: ~,2f~}~^, ~}"
                                  tag
                                  total
                                  (hash-table-count counts)
                                  a
                                  (std-dev (mapcar #'first params))
                                  b
                                  (std-dev (mapcar #'second params))
                                  (exp a)
                                  (* ones (exp a))				
                                  (mapcar (lambda (x)
                                            (list x (adjust-count x a b)))
                                          '(1 2 3 4 5 6))))
                        (setf (gethash :unk counts)
                          (* ones (exp a)))))))))))

  
(defun fudge-smoothing (coc-vector)
  (loop
      with avgs
      for x1 = (first (first coc-vector)) then x2
      for y1 = (log (second (first coc-vector))) then (log  y2)
      for (x2 y2) in (rest coc-vector)
      for slope = (/ (float (- (log y2) y1))
                     (float (- (log x2) (log x1))))
      for offset = (- (log y2) (* (log x2) slope))
      do (push (list slope offset) avgs)	   
      finally (return avgs)))

(defun avg-list (list)
  (loop
      for x in list
      for i from 1
      sum x into accu
      finally (return (/ accu i))))

(defun std-dev (values)
  (loop
      with average = (avg-list values)
      for x in values
      for n from 1
      summing (expt (- x average) 2) into res
      finally (return (sqrt (/ res n)))))

(defun tag-split (tag counts &optional replacement-table)
  (let* ((total (hash-table-sum counts))
         (types (hash-table-count counts)))
    (format t "~&Considering tag-splitting `~a' with ~a types and ~a total" tag types total)
    (cond ((> types 50)
           (format t "~&Too many types, tag-split rejected"))
          ((< types 2)
           (format t "~&Too few types, tag-split rejected"))
          (t
           (let* ((coc-table (coc-table counts))
                  (coc-list (coc-list coc-table))
                  (ll (avg-list (mapcar #'second (fudge-smoothing coc-list)))))
             (format t "~&LL number: ~a" ll)
             (if (< ll -1.0)
                 (format t "~& LL is too low for splitting")
               (loop
                   with replacement-table = (or replacement-table (make-hash-table :test #'equal))
                   for form being the hash-keys in counts
                   for count = (gethash form counts)
                   when (> count 75) do
                     (setf (gethash (list form tag) replacement-table)
                       (format nil "~a|~a" (code-to-token form) tag))
                     (format t "~&Splitted ~a into new tag" (code-to-token form))
                   finally (return replacement-table))))))))

(defun create-tag-split-table (corpus)
  (let ((hmm (setup-hmm (make-hmm)
                        (corpus-tag-set-size corpus)))
        (replacement-table (make-hash-table :test #'equal)))
    (populate-counts corpus hmm)
    (loop
        for counts across (hmm-emissions hmm)
        for tag in (hmm-tags hmm)
        do (tag-split tag counts replacement-table))
    replacement-table))

(defun tag-split-corpora (train test)
  (let ((replacement-table (create-tag-split-table train)))
    (flet ((replacer (corp)
             (loop for sentence in corp
                 collecting
                   (loop 
                       for (form tag) in sentence
                       for replacement = (gethash (list form tag) replacement-table)
                       if replacement 
                       collect (list form replacement)
                       else collect (list form tag)))))
      (list
       (replacer train)
       (replacer test)))))

