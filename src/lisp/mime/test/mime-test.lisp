(in-package :cl-user)

(defpackage :mime-test
  (:use :common-lisp :mime :lift))

(in-package :mime-test)

(defparameter *package-path*
  (directory-namestring
   (asdf:component-pathname (asdf:find-system :mime-test))))

(defun almost= (a b)  
   (< (abs (- a b)) 0.000001))

;; top level test definitions
(deftestsuite all () ())

(deftestsuite regression (all) ()
  (:tests
   ((let ((result (ensure (mime::perform-experiment (merge-pathnames "brown.expr"
                                                                     *package-path*))))
          (known-values '(95.868126 95.94203 95.95978 96.10878 95.522576
                                    95.88038 95.81569 95.87732 95.68882 95.89907)))
      (loop for fold in (mime::profile-folds result)
            for val in known-values
            do (ensure-same (float (mime::fold-token-acc fold)) val :test #'almost=))))))

(deftestsuite experiment (all) ()
  (:tests
   ((mime::with-experiment (e (merge-pathnames "brown.expr" *package-path*))
      (ensure-same (mime::experiment-name e) "BROWN" :test #'equal)))))

(defun run-all-tests ()
  (run-tests :suite 'all))
