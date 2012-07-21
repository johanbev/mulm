(in-package :cl-user)

(defpackage :mime-test
  (:use :common-lisp :mime :lift))

(in-package :mime-test)

(defparameter *package-path*
  (directory-namestring
   (asdf:component-pathname (asdf:find-system :mime-test))))

;; top level test definitions
(deftestsuite all () ())

(defun run-all-tests ()
  (run-tests :suite 'all))
