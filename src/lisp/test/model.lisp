(in-package :mulm-test)


(deftestsuite simple-model
    (all) (hmm)
    (:setup
     (setf hmm (mulm::train (mulm::read-tt-corpus (merge-pathnames "debug.tt" mulm::*eval-path*)))))
    (:tests
     ((ensure-same (mulm::hmm-token-count hmm) 18))
     (ensure-same (mulm::hmm-n hmm) 2)))

(deftestsuite ensure-tag-lm-consistency
    (all) (hmm stream)
    (:test
     (ensure-same (with-output-to-string (stream)
                    (let ((*standard-output* stream))
                      (mulm::check-lm
                       (mulm::read-tt-corpus (merge-pathnames "wsj/wsj.tt" mulm::*eval-path*)))))
                  "")))
