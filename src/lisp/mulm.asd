(defsystem :mulm
  :description "Mulm natural language processing system."
  :version "0.1"
  :author "Johan Benum Evensberget, André Lynum"
  :license ""
  :components ((:file "mulm")
               (:file "input" :depends-on ("symbol-table"))
               (:file "stats" :depends-on ("mulm" "input"))
               (:file "symbol-table" :depends-on ("mulm"))
               (:file "hmm" :depends-on ("symbol-table" "input"))
               (:file "evaluate" :depends-on ("hmm")))
  :depends-on ("cl-heap" "split-sequence" "cl-ppcre"))
