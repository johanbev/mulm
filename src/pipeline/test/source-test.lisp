(in-package :mulm-test)

(defparameter *conll-input-string*
  (format nil "ba~afoo~%knark~abork~%~%frob~afnib~%" #\Tab #\Tab #\Tab))

(defun get-word (sentences sent-index word-index)
  (elt (rest (assoc :words (elt sentences sent-index))) word-index))

(deftestsuite source (all)
  (source sentences)
  (:setup
   (setf source (make-instance 'mulm::conll-source
                               :str (make-string-input-stream *conll-input-string*)))
   (setf sentences (mulm::all-sentences source)))
  (:tests
   ((ensure (listp sentences))
    (ensure-same (length sentences) 2)
    
    (ensure-same (rest (assoc :index (first sentences))) 0)
    (ensure-same (rest (assoc :index (second sentences))) 1)

    (ensure-same (length (rest (assoc :words (first sentences)))) 2)
    (ensure-same (length (rest (assoc :words (second sentences)))) 1)
    (ensure-same (rest (assoc :form (get-word sentences 0 0))) "ba")
    (ensure-same (rest (assoc :pos (get-word sentences 0 0))) "foo")
    (ensure-same (rest (assoc :form (get-word sentences 0 1))) "knark")
    (ensure-same (rest (assoc :pos (get-word sentences 0 1))) "bork")
    (ensure-same (rest (assoc :form (get-word sentences 1 0))) "frob")
    (ensure-same (rest (assoc :pos (get-word sentences 1 0))) "fnib"))
   )
   
  (:teardown
   (close source)))
