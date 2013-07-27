;; Delivery script for Lispworks 6.0
(in-package :cl-user)

(load-all-patches)

;; load and setup asdf
(load (compile-file "asdf-setup.lisp"))

(asdf:operate 'asdf:load-op :mulm :force t)
(asdf:operate 'asdf:load-op :getopt :force t)

(load (compile-file "delivery-top-level.lisp"))

(deliver 'main "mulm-lw-test" 1
         :console :input
         :interface nil)
