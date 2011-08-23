(in-package :mulm-test)

(defparameter *wsj-order2-expr-file*
  (merge-pathnames "wsj-order2.expr" *package-path*))

(defun almost= (x y &key (epsilon 0.0001))
  (< (abs (- x y)) epsilon))

(deftestsuite regression (all-regression)
              ((profile (mime::perform-experiment *wsj-order2-expr-file*)))
  (:tests ((loop for fold in (mime::profile-folds profile)
                   for correct in '(91829 91809 92147 92090 92008 91860 91347 92755 92258 93380)
                   do (ensure-same (mime::fold-token-correct fold) correct)))))
