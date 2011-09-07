(in-package :mulm-test)

(deftestsuite input (all) ()
  (:tests
   (check-number-of-sentences-read
    (ensure-same (length
                  (with-open-file (s mulm::*wsj-eval-file*)
                    (iterate:iter
                      (iterate:generate sent :in-corpus-stream s)
                      (iterate:collect (iterate:next sent)))))
                 500))))
