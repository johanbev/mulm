(defsystem :mulm-test
  :description "Tests for Mulm natural language processing system."
  :version "0.1"
  :author "Johan Benum Evensberget, Andre Lynum"
  :license "GPL"
  :components ((:file "mulm-test")
               (:file "regression" :depends-on ("mulm-test"))
               (:file "model" :depends-on ("mulm-test"))
               (:file "input-test" :depends-on ("mulm-test"))
               (:file "utilities-test" :depends-on ("mulm-test"))
               (:file "../../../pipeline/test/source-test" :depends-on ("mulm-test")))
  :depends-on ("mulm" "mime" "lift"))
