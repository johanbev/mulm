(defsystem :mulm
  :description "Mulm natural language processing system."
  :version "0.1"
  :author "Johan Benum Evensberget, André Lynum"
  :license "GPL"
  :components ((:file "mulm")
               (:file "queue" :depends-on ("mulm"))
               (:file "trie" :depends-on ("mulm"))
               (:file "input" :depends-on ("symbol-table"))
               (:file "stats" :depends-on ("mulm" "input"))
               (:file "symbol-table" :depends-on ("mulm"))
               (:file "hmm" :depends-on ("symbol-table" "input" "trie"))
               (:file "heap" :depends-on ("mulm"))
               (:file "kn" :depends-on ("stats" "hmm"))
               (:file "gt" :depends-on ("stats" "input" "hmm"))
               (:file "best-first-decode" :depends-on ("heap" "symbol-table" "input" "hmm"))
               (:file "evaluate" :depends-on ("hmm")))
  :depends-on ("split-sequence" "cl-ppcre"))
