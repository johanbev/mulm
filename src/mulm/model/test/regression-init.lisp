; This file rus the full regression suite when loaded.
; Run from the shell through regression.sh

(in-package :cl-user)

#+lispworks
(load-all-patches)

;; load and setup asdf
(load (compile-file "../../delivery/asdf-setup.lisp"))

(asdf:operate 'asdf:load-op :mulm-test :force t)

(let ((result (mulm-test::run-regression)))
  (describe result)

  (if (not (null (lift:errors result)))
    (mulm::quit :status 1)
    (mulm::quit :status 0)))
