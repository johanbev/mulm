(defsystem :mulm
  :description "Mulm natural language processing system."
  :version "0.1"
  :author "Johan Benum Evensberget, Andre Lynum"
  :license "GPL"
  :components ((:file "mulm")
               (:file "logging" :depends-on ("mulm"))
               (:file "utilities" :depends-on ("mulm"))
               (:file "lash" :depends-on ("mulm"))
               (:file "count-tree" :depends-on ("mulm" "fast-queue" "utilities" "lash"))
               (:file "fast-queue" :depends-on ("mulm"))
               (:file "input" :depends-on ("lexicon"))
               (:file "lexicon" :depends-on ("mulm"))
               (:file "hmm-model" :depends-on ("lexicon"
                                               "input"
                                               "utilities"
                                               "word-model"))
               (:file "hmm-model-serialization" :depends-on ("hmm-model"))
               (:file "hmm-model-diff" :depends-on ("hmm-model"))
               (:file "hmm-decoder" :depends-on ("hmm-model"))
               (:file "heap" :depends-on ("mulm"))
               (:file "kn" :depends-on ("utilities" "count-tree"))
               (:file "word-model" :depends-on ("kn" "count-tree"))
               (:file "gt" :depends-on ("hmm-model"))
               (:file "best-first-decode" :depends-on ("heap" "lexicon" "input" "hmm-model"))
               (:file "evaluate" :depends-on ("hmm-decoder")))
  :depends-on ("cl-ppcre" "split-sequence" "log5"))
