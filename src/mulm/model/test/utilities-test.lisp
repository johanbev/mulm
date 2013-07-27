(in-package :mulm-test)

(deftestsuite utilities (all) ()
  (:tests
   (tally
    (ensure-same (sort (mulm::tally '(1 2 2 3 3 3 4 4 4 4)) #'< :key #'first)
                 '((1 . 1) (2 . 2) (3 . 3) (4 . 4))))))
