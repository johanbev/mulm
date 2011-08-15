;; Delivery script for Lispworks 6.0
(in-package :cl-user)

;; replace with your path to asdf
(load (compile-file "../../../config/asdf.lisp"))

;; replace with your path to asdf systems
(push "../../../asdf-systems/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op :mulm :force t)

(load (compile-file "delivery-top-level.lisp"))

;; dummy main function giving the code a short workout
(save-lisp-and-die #p"mulm-sbcl-test" :executable t :toplevel #'main)
