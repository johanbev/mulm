(defsystem :mulm
  :description "Mulm natural language processing system."
  :version "0.1"
  :author "Johan Benum Evensberget, André Lynum"
  :license ""
  :components ((:file "mulm")
               (:file "symbol-table" :depends-on ("mulm"))
               (:file "hmm" :depends-on ("symbol-table")))
  :depends-on ())