(defsystem :mulm
  :description "Mulm natural language processing system."
  :version "0.1"
  :author "Johan Benum Evensberget, Andre Lynum"
  :license "GPL"
  :components ((:file "mulm")
               (:file "utilities" :depends-on ("mulm"))
               (:file "count-tree" :depends-on ("mulm" "queue" "utilities"))
               (:file "queue" :depends-on ("mulm"))
               (:file "trie" :depends-on ("utilities"))
               (:file "input" :depends-on ("lexicon"))
               (:file "stats" :depends-on ("mulm" "input" "utilities" "hmm-model"))
               (:file "lexicon" :depends-on ("mulm"))
               (:file "hmm-model" :depends-on ("lexicon"
                                               "input"
                                               "trie"
                                               "utilities"
                                               "word-model"))
               (:file "hmm-decoder" :depends-on ("hmm-model"))
               (:file "heap" :depends-on ("mulm"))
               (:file "kn" :depends-on ("utilities" "count-tree"))
               (:file "word-model" :depends-on ("kn" "count-tree"))
               (:file "gt" :depends-on ("stats"))
               (:file "best-first-decode" :depends-on ("heap" "lexicon" "input" "hmm-model"))
               (:file "evaluate" :depends-on ("hmm-decoder")))
  :depends-on ("split-sequence" "cl-ppcre"))
