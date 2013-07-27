(defsystem :mulm
  :description "Mulm natural language processing system."
  :version "0.2"
  :author "Johan Benum Evensberget, Andre Lynum"
  :license "AGPL"
  :components ((:module :base :pathname "" 
                        :components
                        ((:file "mulm")))
               (:module :misc :pathname "misc" :depends-on (:base)
                        :components
                        ((:file "logging")
                         (:file "utilities")
                         (:module :datastructures 
                                  :pathname "datastructures"
                                  :components
                                  ((:file "lash")
                                   (:file "heap")
                                   (:file "fast-queue")
                                   (:file "count-tree" :depends-on ("fast-queue" "heap" "lash"))))
                         (:module :math
                                  :pathname "math"
                                  :components
                                  ((:file "stats")))
                         (:file "input" :depends-on ("lexicon"))
                         (:file "lexicon")))
               (:module :model
                        :pathname "model"
                        :depends-on (:base :misc)
                        :components
                        ((:file "kn")
                         (:file "gt")
                         (:file "word-model" :depends-on ("kn"))
                         (:file "hmm-model" :depends-on ("word-model"))
                         (:file "hmm-model-serialization" :depends-on ("hmm-model"))
                         (:file "hmm-model-diff" :depends-on ("hmm-model"))
                         (:file "hmm-decoder" :depends-on ("hmm-model"))
                         (:file "best-first-decode" :depends-on ("hmm-model"))))
               (:module :pipeline :pathname "../pipeline"
                        :components
                        ((:file "source"))))
              ; (:file "evaluate" :depends-on ("hmm-decoder"))
              ; (:file "ig-interpolation" :depends-on ("hmm-model" "lash")))
  :depends-on ("cl-ppcre" "split-sequence" "log5" "iterate"))
