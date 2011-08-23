(in-package :cl-user)

(defpackage :mulm-test
  (:use :common-lisp :mulm :lift))

(in-package :mulm-test)

(defparameter *package-path*
  (directory-namestring
   (asdf:component-pathname (asdf:find-system :mulm-test))))

;; top level test definitions
(deftestsuite all-regression () ())
(deftestsuite all (all-regression) ())

(defun run-all-tests ()
  (run-tests :suite 'all))

(defun run-regression ()
  (run-tests :suite 'all-regression))
