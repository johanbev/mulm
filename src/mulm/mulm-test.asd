(defsystem :mulm-test
  :components ((:module :base :pathname "test"
                        :components ((:file "mulm-test")))
               (:module :misc :pathname "misc/test"
                        :components ((:file "test-feature-factor"))))
  :depends-on (:mulm :lisp-unit))
