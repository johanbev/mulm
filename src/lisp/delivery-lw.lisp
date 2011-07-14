;; Delivery script for Lispworks 6.0
(in-package :cl-user)

(load-all-patches)

;; replace with your path to asdf
(load (compile-file "../../../config/asdf.lisp"))

;; replace with your path to asdf systems
(push "../../../asdf-systems/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op :mulm)

;; dummy main function giving the code a short workout
(defun main ()
  (mulm::do-evaluation))

(deliver 'main "mulm-test" 2
         :console :input
         :interface nil)
