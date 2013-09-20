(in-package :mulm-test)

(defparameter *features1*
  `((:tag-1-tag-2 ,(mulm::make-feature-representation :tag-1-tag-2 :word nil :tag-1 t :tag-2 t))
    (:word ,(mulm::make-feature-representation :word))))

(defparameter *sentence1*
  '(("How" "WRB") ("are" "VBP") ("you" "PP") ("friend" "NN") ("?" "?")))

(define-test test-make-suffix-extractor
  (let ((extractor (mulm::make-suffix-extractor 1)))
    (assert-equal "" (funcall extractor ""))
    (assert-equal "1" (funcall extractor "1"))
    (assert-equal "2" (funcall extractor "12"))
    (assert-equal "3" (funcall extractor "123"))
    (assert-equal "4" (funcall extractor "1234")))
  (let ((extractor (mulm::make-suffix-extractor 2)))
    (assert-equal "" (funcall extractor ""))
    (assert-equal "1" (funcall extractor "1"))
    (assert-equal "12" (funcall extractor "12"))
    (assert-equal "23" (funcall extractor "123"))
    (assert-equal "34" (funcall extractor "1234")))
  (let ((extractor (mulm::make-suffix-extractor)))
    (assert-equal "" (funcall extractor ""))
    (assert-equal "1" (funcall extractor "1"))
    (assert-equal "12" (funcall extractor "12"))
    (assert-equal "123" (funcall extractor "123"))
    (assert-equal "234" (funcall extractor "1234"))))

(define-test test-make-prefix-extractor
  (let ((extractor (mulm::make-prefix-extractor)))
    (assert-equal "" (funcall extractor ""))
    (assert-equal "1" (funcall extractor "1"))
    (assert-equal "1" (funcall extractor "12"))
    (assert-equal "1" (funcall extractor "123"))
    (assert-equal "1" (funcall extractor "1234")))
  (let ((extractor (mulm::make-prefix-extractor 2)))
    (assert-equal "" (funcall extractor ""))
    (assert-equal "1" (funcall extractor "1"))
    (assert-equal "12" (funcall extractor "12"))
    (assert-equal "12" (funcall extractor "123"))
    (assert-equal "12" (funcall extractor "1234"))))

(define-test test-feature-representation-suffix
  (let ((context (mapcar #'first *sentence1*)))
    (let ((repr (mulm::make-feature-representation :suffix
                                                   :extractor (mulm::make-suffix-extractor))))
      (assert-equal '(:suffix :|How|) (funcall repr context 0 "<s>" "<s>"))
      (assert-equal '(:suffix :|are|) (funcall repr context 1 "WRB" "<s>"))
      (assert-equal '(:suffix :|you|) (funcall repr context 2 "VBP" "WRB"))
      (assert-equal '(:suffix :|end|) (funcall repr context 3 "PP" "VBP"))
      (assert-equal '(:suffix :|?|) (funcall repr context 4 "NN" "PP"))
      (assert-true (null (funcall repr context 5 "?" "NN"))))
    (let ((repr (mulm::make-feature-representation :suffix
                                                   :extractor (mulm::make-suffix-extractor 2))))
      (assert-equal '(:suffix :|ow|) (funcall repr context 0 "<s>" "<s>"))
      (assert-equal '(:suffix :|re|) (funcall repr context 1 "WRB" "<s>"))
      (assert-equal '(:suffix :|ou|) (funcall repr context 2 "VBP" "WRB"))
      (assert-equal '(:suffix :|nd|) (funcall repr context 3 "PP" "VBP"))
      (assert-equal '(:suffix :|?|) (funcall repr context 4 "NN" "PP"))
      (assert-true (null (funcall repr context 5 "?" "NN"))))))

(define-test test-feature-representation-prefix
  (let ((context (mapcar #'first *sentence1*)))
    (let ((repr (mulm::make-feature-representation :prefix
                                                   :extractor (mulm::make-prefix-extractor))))
      (assert-equal '(:prefix :|H|) (funcall repr context 0 "<s>" "<s>"))
      (assert-equal '(:prefix :|a|) (funcall repr context 1 "WRB" "<s>"))
      (assert-equal '(:prefix :|y|) (funcall repr context 2 "VBP" "WRB"))
      (assert-equal '(:prefix :|f|) (funcall repr context 3 "PP" "VBP"))
      (assert-equal '(:prefix :|?|) (funcall repr context 4 "NN" "PP"))
      (assert-true (null (funcall repr context 5 "?" "NN"))))
    (let ((repr (mulm::make-feature-representation :prefix
                                                   :extractor (mulm::make-prefix-extractor 2))))
      (assert-equal '(:prefix :|Ho|) (funcall repr context 0 "<s>" "<s>"))
      (assert-equal '(:prefix :|ar|) (funcall repr context 1 "WRB" "<s>"))
      (assert-equal '(:prefix :|yo|) (funcall repr context 2 "VBP" "WRB"))
      (assert-equal '(:prefix :|fr|) (funcall repr context 3 "PP" "VBP"))
      (assert-equal '(:prefix :|?|) (funcall repr context 4 "NN" "PP"))
      (assert-true (null (funcall repr context 5 "?" "NN"))))))

(define-test test-feature-representation-tag-1
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :tag-1 :word nil :tag-1 t)))
    (assert-equal '(:tag-1 :|<s>|) (funcall repr context 0 "<s>" "<s>"))
    (assert-equal '(:tag-1 :|WRB|) (funcall repr context 1 "WRB" "<s>"))
    (assert-equal '(:tag-1 :|VBP|) (funcall repr context 2 "VBP" "WRB"))
    (assert-equal '(:tag-1 :|PP|) (funcall repr context 3 "PP" "VBP"))
    (assert-equal '(:tag-1 :|NN|) (funcall repr context 4 "NN" "PP"))
    (assert-true (null (funcall repr context 5 "?" "NN")))
    (assert-true (null (funcall repr context 6 nil "?")))))

(define-test test-feature-representation-tag-2
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :tag-2 :word nil :tag-2 t)))
    (assert-equal '(:tag-2 :|<s>|) (funcall repr context 0 "<s>" "<s>"))
    (assert-equal '(:tag-2 :|<s>|) (funcall repr context 1 "WRB" "<s>"))
    (assert-equal '(:tag-2 :|WRB|) (funcall repr context 2 "VBP" "WRB"))
    (assert-equal '(:tag-2 :|VBP|) (funcall repr context 3 "PP" "VBP"))
    (assert-equal '(:tag-2 :|PP|) (funcall repr context 4 "NN" "PP"))
    (assert-true (null (funcall repr context 5 "?" "NN")))
    (assert-true (null (funcall repr context 6 nil "?")))
    (assert-true (null (funcall repr context 7 nil nil)))))

(define-test test-feature-representation-tag-1-tag-2
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :tag-1-tag-2 :word nil :tag-1 t :tag-2 t)))
    (assert-equal '(:tag-1-tag-2 :|<s>| :|<s>|) (funcall repr context 0 "<s>" "<s>"))
    (assert-equal '(:tag-1-tag-2 :|WRB| :|<s>|) (funcall repr context 1 "WRB" "<s>"))
    (assert-equal '(:tag-1-tag-2 :|VBP| :|WRB|) (funcall repr context 2 "VBP" "WRB"))
    (assert-equal '(:tag-1-tag-2 :|PP| :|VBP|) (funcall repr context 3 "PP" "VBP"))
    (assert-equal '(:tag-1-tag-2 :|NN| :|PP|) (funcall repr context 4 "NN" "PP"))
    (assert-true (null (funcall repr context 5 "?" "NN")))
    (assert-true (null (funcall repr context 6 nil "?")))))

(define-test test-feature-representation-word
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :word)))
    (assert-equal '(:word :|How|) (funcall repr context 0 "<s>" "<s>"))
    (assert-equal '(:word :|are|) (funcall repr context 1 "WRB" "<s>"))
    (assert-equal '(:word :|you|) (funcall repr context 2 "VBP" "WRB"))
    (assert-equal '(:word :|friend|) (funcall repr context 3 "PP" "VBP"))
    (assert-equal '(:word :|?|) (funcall repr context 4 "NN" "PP"))
    (assert-true (null (funcall repr context 5 "?" "NN")))))

(define-test test-feature-representation-word-tag-1
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :word-tag-1 :tag-1 t)))
    (assert-equal '(:word-tag-1 :|How| :|<s>|) (funcall repr context 0 "<s>" "<s>"))
    (assert-equal '(:word-tag-1 :|are| :|WRB|) (funcall repr context 1 "WRB" "<s>"))
    (assert-equal '(:word-tag-1 :|you| :|VBP|) (funcall repr context 2 "VBP" "WRB"))
    (assert-equal '(:word-tag-1 :|friend| :|PP|) (funcall repr context 3 "PP" "VBP"))
    (assert-equal '(:word-tag-1 :|?| :|NN|) (funcall repr context 4 "NN" "PP"))
    (assert-true (null (funcall repr context 5 "?" "NN")))))

(define-test test-feature-representation-word-1
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :word-1 :offset -1)))
    (assert-true (null (funcall repr context 0 "<s>" "<s>")))
    (assert-equal '(:word-1 :|How|) (funcall repr context 1 "WRB" "<s>"))
    (assert-equal '(:word-1 :|are|) (funcall repr context 2 "VBP" "WRB"))
    (assert-equal '(:word-1 :|you|) (funcall repr context 3 "PP" "VBP"))
    (assert-equal '(:word-1 :|friend|) (funcall repr context 4 "NN" "PP"))
    (assert-true (null (funcall repr context 5 "?" "NN")))))

(define-test test-feature-representation-suffix-1
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :suffix-1 :offset -1
                                                 :extractor (mulm::make-suffix-extractor))))
    (assert-true (null (funcall repr context 0 "<s>" "<s>")))
    (assert-equal '(:suffix-1 :|How|) (funcall repr context 1 "WRB" "<s>"))
    (assert-equal '(:suffix-1 :|are|) (funcall repr context 2 "VBP" "WRB"))
    (assert-equal '(:suffix-1 :|you|) (funcall repr context 3 "PP" "VBP"))
    (assert-equal '(:suffix-1 :|end|) (funcall repr context 4 "NN" "PP"))
    (assert-true (null (funcall repr context 5 "?" "NN")))))

(define-test test-feature-representation-word-2
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :word-2 :offset -2)))
    (assert-true (null (funcall repr context 0 "<s>" "<s>")))
    (assert-true (null (funcall repr context 1 "WRB" "<s>")))
    (assert-equal '(:word-2 :|How|) (funcall repr context 2 "VBP" "WRB"))
    (assert-equal '(:word-2 :|are|) (funcall repr context 3 "PP" "VBP"))
    (assert-equal '(:word-2 :|you|) (funcall repr context 4 "NN" "PP"))
    (assert-true (null (funcall repr context 5 "?" "NN")))))

(define-test test-feature-representation-word+1
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :word+1 :offset 1)))
    (assert-equal '(:word+1 :|are|) (funcall repr context 0 "<s>" "<s>"))
    (assert-equal '(:word+1 :|you|) (funcall repr context 1 "WRB" "<s>"))
    (assert-equal '(:word+1 :|friend|) (funcall repr context 2 "VBP" "WRB"))
    (assert-equal '(:word+1 :|?|) (funcall repr context 3 "PP" "VBP"))
    (assert-true (null (funcall repr context 4 "NN" "PP")))
    (assert-true (null (funcall repr context 5 "?" "NN")))))

(define-test test-feature-representation-suffix+1
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :suffix+1 :offset 1
                                                 :extractor (mulm::make-suffix-extractor))))
    (assert-equal '(:suffix+1 :|are|) (funcall repr context 0 "<s>" "<s>"))
    (assert-equal '(:suffix+1 :|you|) (funcall repr context 1 "WRB" "<s>"))
    (assert-equal '(:suffix+1 :|end|) (funcall repr context 2 "VBP" "WRB"))
    (assert-equal '(:suffix+1 :|?|) (funcall repr context 3 "PP" "VBP"))
    (assert-true (null (funcall repr context 4 "NN" "PP")))
    (assert-true (null (funcall repr context 5 "?" "NN")))))

(define-test test-feature-representation-word+2
  (let ((context (mapcar #'first *sentence1*))
        (repr (mulm::make-feature-representation :word+2 :offset 2)))
    (assert-equal '(:word+2 :|you|) (funcall repr context 0 "<s>" "<s>"))
    (assert-equal '(:word+2 :|friend|) (funcall repr context 1 "WRB" "<s>"))
    (assert-equal '(:word+2 :|?|) (funcall repr context 2 "VBP" "WRB"))
    (assert-true (null (funcall repr context 3 "PP" "VBP")))
    (assert-true (null (funcall repr context 4 "NN" "PP")))
    (assert-true (null (funcall repr context 5 "?" "NN")))))

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
    (assert-true (= 1 (mulm::get-feature-count :word '(:word :|How|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :word '(:word :|are|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :word '(:word :|you|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :word '(:word :|?|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :tag-1-tag-2 '(:tag-1-tag-2 :|<s>| :|<s>|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :tag-1-tag-2 '(:tag-1-tag-2 :|WRB| :|<s>|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :tag-1-tag-2 '(:tag-1-tag-2 :|VBP| :|WRB|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :tag-1-tag-2 '(:tag-1-tag-2 :|PP| :|VBP|) counts)))
    (assert-true (= 1 (mulm::get-feature-count :tag-1-tag-2 '(:tag-1-tag-2 :|NN| :|PP|) counts)))))
