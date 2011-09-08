(in-package :mulm-test)

(deftestsuite input (all) ()
  (:tests
   (check-number-of-sentences-read
    (ensure-same (length (mulm::read-tt-corpus mulm::*wsj-eval-file*))
                 500))))
