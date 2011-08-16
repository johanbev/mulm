;; Delivery script for Lispworks 6.0
(in-package :cl-user)

(load-all-patches)

;; replace with your path to asdf
(load (compile-file "../../../config/asdf.lisp"))

;; replace with your path to asdf systems
(push "../../../asdf-systems/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op :mulm :force t)
(asdf:operate 'asdf:load-op :getopt :force t)

(load (compile-file "delivery-top-level.lisp"))

(deliver 'main "mulm-lw-test" 2
         :console :input
         :interface nil)
