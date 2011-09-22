(in-package :mulm-test)

(defparameter *wsj-order2-expr-file*
  (merge-pathnames "wsj-order2.expr" *package-path*))

(defun almost= (x y &key (epsilon 0.0001))
  (< (abs (- x y)) epsilon))

(deftestsuite regression (all-regression)
              ((profile (mime::perform-experiment *wsj-order2-expr-file*)))
  (:tests ((loop for fold in (mime::profile-folds profile)
                   for correct in '(91858 91831 92161 92103 92022 91880 91357 92769 92272 93393)
                   do (ensure-same (mime::fold-token-correct fold) correct)))))
