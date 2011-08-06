(defsystem :mulm-test
  :description "Tests for Mulm natural language processing system."
  :version "0.1"
  :author "Johan Benum Evensberget, Andre Lynum"
  :license "GPL"
  :components ((:file "mulm-test")
               (:file "regression" :depends-on ("mulm-test"))
               (:file "model" :depends-on ("mulm-test")))
  :depends-on ("mulm" "lift"))
