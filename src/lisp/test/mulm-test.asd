(defsystem :mulm-test
  :description "Tests for Mulm natural language processing system."
  :version "0.1"
  :author "Johan Benum Evensberget, André Lynum"
  :license "GPL"
  :components ((:file "mulm-test")
               (:file "regression" :depends-on ("mulm-test")))
  :depends-on ("mulm" "lift"))
