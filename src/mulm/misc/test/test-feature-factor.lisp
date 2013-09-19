(in-package :mulm-test)

(defparameter *features1*
  `((:bigram ,#'mulm::bigram-extractor)
    (:emission ,#'mulm::emission-extractor)))

(defparameter *sentence1*
  '(("How" "WRB") ("are" "VBP") ("you" "PP") ("?" "?")))

(define-test test-emission-extractor
  (assert-equal '(:|How| :|WRB|) (mulm::emission-extractor *sentence1* 0))
  (assert-equal '(:|are| :|VBP|) (mulm::emission-extractor *sentence1* 1))
  (assert-equal '(:|you| :|PP|) (mulm::emission-extractor *sentence1* 2))
  (assert-equal '(:|?| :|?|) (mulm::emission-extractor *sentence1* 3)))

(define-test test-bigram-extractor
  (assert-equal '(:|<s>| :|WRB|) (mulm::bigram-extractor *sentence1* 0))
  (assert-equal '(:|<s>| :|WRB|) (mulm::bigram-extractor *sentence1* 1))
  (assert-equal '(:|<s>| :|WRB|) (mulm::bigram-extractor *sentence1* 2))
  (assert-equal '(:|<s>| :|WRB|) (mulm::bigram-extractor *sentence1* 3)))

(define-test test-add-feature-count
  (let ((counts (make-hash-table)))
    (mulm::add-feature-count :id "val" counts)
    (assert-true (= 1 (mulm::get-feature-count :id "val" counts)))
    (mulm::add-feature-count :id "val" counts)
    (assert-true (= 2 (mulm::get-feature-count :id "val" counts)))
    (assert-true (null (mulm::get-feature-count :other-id "val" counts)))
    (assert-true (null (mulm::get-feature-count :id "other-val" counts)))))

(define-test test-count-features
  (let ((counts (make-hash-table)))
    (assert-equal 'hash-table (type-of (mulm::count-features *sentence1* *features1* counts)))
    (assert-true (= 1 (mulm::get-feature-count :emission '(:|How| :|WRB|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :emission '(:|are| :|VBP|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :emission '(:|you| :|PP|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :emission '(:|?| :|?|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :bigram '(:|<s>| :|WRB|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :bigram '(:|WRB| :|VBP|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :bigram '(:|VBP| :|PP|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :bigram '(:|PP| :|?|) counts)))))
