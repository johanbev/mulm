(in-package :mulm-test)

(defun almost= (x y &key (epsilon 0.0001))
  (< (abs (- x y)) epsilon))

(deftestsuite regression () ())

(addtest (regression)
  wsj-regression
  (ensure-same (mulm::do-evaluation) 0.96585 :test #'almost=))
