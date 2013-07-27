(in-package :mulm-test)

(defparameter *conll-input-string*
  (format nil "ba~afoo~%knark~abork~%~%frob~afnib~%" #\Tab #\Tab #\Tab))

(deftestsuite source (all)
  (source sentences)
  (:setup
   (setf source (make-instance 'mulm::conll-source
                               :str (make-string-input-stream *conll-input-string*)))
   (setf sentences (mulm::all-sentences source)))
  (:tests
   (ensure (listp sentences))
               (ensure-same (length sentences) 2))
  (:teardown
   (close source)))
