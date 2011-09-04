;; Delivery script for Lispworks 6.0
(in-package :cl-user)

;; load and setup asdf
(load "asdf-setup.lisp")

(asdf:operate 'asdf:load-op :mulm :force t)
(asdf:operate 'asdf:load-op :getopt :force t)

(load (compile-file "delivery-top-level.lisp"))

;; dummy main function giving the code a short workout
(save-lisp-and-die #p"mulm-sbcl-test" :executable t :toplevel #'main)
