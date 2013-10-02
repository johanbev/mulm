(in-package :mulm-test)

(defparameter *features1*
  (list
   (mulm::make-feature-representation :tag-1-tag-2 :word nil :tag-1 t :tag-2 t)
   (mulm::make-feature-representation :word)))

(defparameter *features2*
  (list
   (mulm::make-feature-representation :tag-1 :word nil :tag-1 t)
   (mulm::make-feature-representation :word)))

(defparameter *sentence1*
  '(("How" "WRB") ("are" "VBP") ("you" "PP") ("friend" "NN") ("?" "?")))

(defparameter *corpus1*
  '((("x" "1") ("y" "0") ("x" "0") ("x" "1"))
    (("x" "1") ("x" "1") ("y" "0") ("x" "1") ("13" "d"))
    (("x" "1") ("y" "0") ("x" "1") ("x" "1"))
    (("x" "1") ("x" "1") ("y" "0") ("x" "1") ("13" "d"))))
    
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
    (let* ((feature (mulm::make-feature-representation :suffix
                                                       :extractor (mulm::make-suffix-extractor)))
           (feat-id (first feature))
           (feat-ex (second feature)))
      (assert-equal :suffix feat-id)
      (assert-equal '(:suffix :|How|) (funcall feat-ex context 0 "<s>" "<s>"))
      (assert-equal '(:suffix :|are|) (funcall feat-ex context 1 "WRB" "<s>"))
      (assert-equal '(:suffix :|you|) (funcall feat-ex context 2 "VBP" "WRB"))
      (assert-equal '(:suffix :|end|) (funcall feat-ex context 3 "PP" "VBP"))
      (assert-equal '(:suffix :|?|) (funcall feat-ex context 4 "NN" "PP"))
      (assert-true (null (funcall feat-ex context 5 "?" "NN"))))
    (let* ((feature (mulm::make-feature-representation :suffix
                                                       :extractor (mulm::make-suffix-extractor 2)))
           (feat-id (first feature))
           (feat-ex (second feature)))
      (assert-equal :suffix feat-id)
      (assert-equal '(:suffix :|ow|) (funcall feat-ex context 0 "<s>" "<s>"))
      (assert-equal '(:suffix :|re|) (funcall feat-ex context 1 "WRB" "<s>"))
      (assert-equal '(:suffix :|ou|) (funcall feat-ex context 2 "VBP" "WRB"))
      (assert-equal '(:suffix :|nd|) (funcall feat-ex context 3 "PP" "VBP"))
      (assert-equal '(:suffix :|?|) (funcall feat-ex context 4 "NN" "PP"))
      (assert-true (null (funcall feat-ex context 5 "?" "NN"))))))

(define-test test-feature-representation-prefix
  (let ((context (mapcar #'first *sentence1*)))
    (let* ((feature (mulm::make-feature-representation :prefix
                                                       :extractor (mulm::make-prefix-extractor)))
           (feat-id (first feature))
           (feat-ex (second feature)))
      (assert-equal :prefix feat-id)
      (assert-equal '(:prefix :|H|) (funcall feat-ex context 0 "<s>" "<s>"))
      (assert-equal '(:prefix :|a|) (funcall feat-ex context 1 "WRB" "<s>"))
      (assert-equal '(:prefix :|y|) (funcall feat-ex context 2 "VBP" "WRB"))
      (assert-equal '(:prefix :|f|) (funcall feat-ex context 3 "PP" "VBP"))
      (assert-equal '(:prefix :|?|) (funcall feat-ex context 4 "NN" "PP"))
      (assert-true (null (funcall feat-ex context 5 "?" "NN"))))
    (let* ((feature (mulm::make-feature-representation :prefix
                                                       :extractor (mulm::make-prefix-extractor 2)))
           (feat-id (first feature))
           (feat-ex (second feature)))
      (assert-equal :prefix feat-id)
      (assert-equal '(:prefix :|Ho|) (funcall feat-ex context 0 "<s>" "<s>"))
      (assert-equal '(:prefix :|ar|) (funcall feat-ex context 1 "WRB" "<s>"))
      (assert-equal '(:prefix :|yo|) (funcall feat-ex context 2 "VBP" "WRB"))
      (assert-equal '(:prefix :|fr|) (funcall feat-ex context 3 "PP" "VBP"))
      (assert-equal '(:prefix :|?|) (funcall feat-ex context 4 "NN" "PP"))
      (assert-true (null (funcall feat-ex context 5 "?" "NN"))))))

(define-test test-feature-representation-tag-1
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :tag-1 :word nil :tag-1 t))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :tag-1 feat-id)
    (assert-equal '(:tag-1 :|<s>|) (funcall feat-ex context 0 "<s>" "<s>"))
    (assert-equal '(:tag-1 :|WRB|) (funcall feat-ex context 1 "WRB" "<s>"))
    (assert-equal '(:tag-1 :|VBP|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-equal '(:tag-1 :|PP|) (funcall feat-ex context 3 "PP" "VBP"))
    (assert-equal '(:tag-1 :|NN|) (funcall feat-ex context 4 "NN" "PP"))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))
    (assert-true (null (funcall feat-ex context 6 nil "?")))))

(define-test test-feature-representation-tag-2
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :tag-2 :word nil :tag-2 t))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :tag-2 feat-id)
    (assert-equal '(:tag-2 :|<s>|) (funcall feat-ex context 0 "<s>" "<s>"))
    (assert-equal '(:tag-2 :|<s>|) (funcall feat-ex context 1 "WRB" "<s>"))
    (assert-equal '(:tag-2 :|WRB|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-equal '(:tag-2 :|VBP|) (funcall feat-ex context 3 "PP" "VBP"))
    (assert-equal '(:tag-2 :|PP|) (funcall feat-ex context 4 "NN" "PP"))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))
    (assert-true (null (funcall feat-ex context 6 nil "?")))
    (assert-true (null (funcall feat-ex context 7 nil nil)))))

(define-test test-feature-representation-tag-1-tag-2
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :tag-1-tag-2 :word nil :tag-1 t :tag-2 t))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :tag-1-tag-2 feat-id)
    (assert-equal '(:tag-1-tag-2 :|<s>| :|<s>|) (funcall feat-ex context 0 "<s>" "<s>"))
    (assert-equal '(:tag-1-tag-2 :|WRB| :|<s>|) (funcall feat-ex context 1 "WRB" "<s>"))
    (assert-equal '(:tag-1-tag-2 :|VBP| :|WRB|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-equal '(:tag-1-tag-2 :|PP| :|VBP|) (funcall feat-ex context 3 "PP" "VBP"))
    (assert-equal '(:tag-1-tag-2 :|NN| :|PP|) (funcall feat-ex context 4 "NN" "PP"))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))
    (assert-true (null (funcall feat-ex context 6 nil "?")))))

(define-test test-feature-representation-word
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :word))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :word feat-id)
    (assert-equal '(:word :|How|) (funcall feat-ex context 0 "<s>" "<s>"))
    (assert-equal '(:word :|are|) (funcall feat-ex context 1 "WRB" "<s>"))
    (assert-equal '(:word :|you|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-equal '(:word :|friend|) (funcall feat-ex context 3 "PP" "VBP"))
    (assert-equal '(:word :|?|) (funcall feat-ex context 4 "NN" "PP"))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))))

(define-test test-feature-representation-word-tag-1
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :word-tag-1 :tag-1 t))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :word-tag-1 feat-id)
    (assert-equal '(:word-tag-1 :|How| :|<s>|) (funcall feat-ex context 0 "<s>" "<s>"))
    (assert-equal '(:word-tag-1 :|are| :|WRB|) (funcall feat-ex context 1 "WRB" "<s>"))
    (assert-equal '(:word-tag-1 :|you| :|VBP|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-equal '(:word-tag-1 :|friend| :|PP|) (funcall feat-ex context 3 "PP" "VBP"))
    (assert-equal '(:word-tag-1 :|?| :|NN|) (funcall feat-ex context 4 "NN" "PP"))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))))

(define-test test-feature-representation-word-1
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :word-1 :offset -1))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :word-1 feat-id)
    (assert-true (null (funcall feat-ex context 0 "<s>" "<s>")))
    (assert-equal '(:word-1 :|How|) (funcall feat-ex context 1 "WRB" "<s>"))
    (assert-equal '(:word-1 :|are|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-equal '(:word-1 :|you|) (funcall feat-ex context 3 "PP" "VBP"))
    (assert-equal '(:word-1 :|friend|) (funcall feat-ex context 4 "NN" "PP"))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))))

(define-test test-feature-representation-suffix-1
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :suffix-1 :offset -1
                                                     :extractor (mulm::make-suffix-extractor)))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :suffix-1 feat-id)
    (assert-true (null (funcall feat-ex context 0 "<s>" "<s>")))
    (assert-equal '(:suffix-1 :|How|) (funcall feat-ex context 1 "WRB" "<s>"))
    (assert-equal '(:suffix-1 :|are|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-equal '(:suffix-1 :|you|) (funcall feat-ex context 3 "PP" "VBP"))
    (assert-equal '(:suffix-1 :|end|) (funcall feat-ex context 4 "NN" "PP"))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))))

(define-test test-feature-representation-word-2
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :word-2 :offset -2))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :word-2 feat-id)
    (assert-true (null (funcall feat-ex context 0 "<s>" "<s>")))
    (assert-true (null (funcall feat-ex context 1 "WRB" "<s>")))
    (assert-equal '(:word-2 :|How|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-equal '(:word-2 :|are|) (funcall feat-ex context 3 "PP" "VBP"))
    (assert-equal '(:word-2 :|you|) (funcall feat-ex context 4 "NN" "PP"))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))))

(define-test test-feature-representation-word+1
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :word+1 :offset 1))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :word+1 feat-id)
    (assert-equal '(:word+1 :|are|) (funcall feat-ex context 0 "<s>" "<s>"))
    (assert-equal '(:word+1 :|you|) (funcall feat-ex context 1 "WRB" "<s>"))
    (assert-equal '(:word+1 :|friend|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-equal '(:word+1 :|?|) (funcall feat-ex context 3 "PP" "VBP"))
    (assert-true (null (funcall feat-ex context 4 "NN" "PP")))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))))

(define-test test-feature-representation-suffix+1
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :suffix+1 :offset 1
                                                     :extractor (mulm::make-suffix-extractor)))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :suffix+1 feat-id)
    (assert-equal '(:suffix+1 :|are|) (funcall feat-ex context 0 "<s>" "<s>"))
    (assert-equal '(:suffix+1 :|you|) (funcall feat-ex context 1 "WRB" "<s>"))
    (assert-equal '(:suffix+1 :|end|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-equal '(:suffix+1 :|?|) (funcall feat-ex context 3 "PP" "VBP"))
    (assert-true (null (funcall feat-ex context 4 "NN" "PP")))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))))

(define-test test-feature-representation-word+2
  (let* ((context (mapcar #'first *sentence1*))
         (feature (mulm::make-feature-representation :word+2 :offset 2))
         (feat-id (first feature))
         (feat-ex (second feature)))
    (assert-equal :word+2 feat-id)
    (assert-equal '(:word+2 :|you|) (funcall feat-ex context 0 "<s>" "<s>"))
    (assert-equal '(:word+2 :|friend|) (funcall feat-ex context 1 "WRB" "<s>"))
    (assert-equal '(:word+2 :|?|) (funcall feat-ex context 2 "VBP" "WRB"))
    (assert-true (null (funcall feat-ex context 3 "PP" "VBP")))
    (assert-true (null (funcall feat-ex context 4 "NN" "PP")))
    (assert-true (null (funcall feat-ex context 5 "?" "NN")))))

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

(define-test test-feature-factor-initialize-instance
  (let ((factor (make-instance 'mulm::feature-factor
                               :features *features2*
                               :cutoff 0
                               :corpus (subseq *corpus1* 0 3))))
    (with-slots (mulm::feature-index-map mulm::index-feature-map mulm::target-index-map
                 mulm::index-target-map mulm::features mulm::cutoff
                 mulm::p mulm::w mulm::c)
        factor
      (assert-true (equal '(0 1 2 3 4 5)
                          (sort
                           (remove-duplicates (loop for index being the hash-values in mulm::feature-index-map
                                                    collect index)
                                              :test #'equal)
                           #'<)))
      (assert-true (find '(:word :|x|) (loop for feature being the hash-keys in mulm::feature-index-map
                                             collect feature)
                         :test #'equal))
      (assert-true (find '(:word :|y|) (loop for feature being the hash-keys in mulm::feature-index-map
                                             collect feature)
                         :test #'equal))
      (assert-true (find '(:word :|13|) (loop for feature being the hash-keys in mulm::feature-index-map
                                              collect feature)
                         :test #'equal))
      (assert-true (find '(:tag-1 :|1|) (loop for feature being the hash-keys in mulm::feature-index-map
                                              collect feature)
                         :test #'equal))
      (assert-true (find '(:tag-1 :|0|) (loop for feature being the hash-keys in mulm::feature-index-map
                                              collect feature)
                         :test #'equal))
      (assert-true (find '(:tag-1 :|<s>|) (loop for feature being the hash-keys in mulm::feature-index-map
                                                collect feature)
                         :test #'equal))
      (assert-true (= 6 (length mulm::index-feature-map)))
      (assert-true (find '(:word :|x|) mulm::index-feature-map :test #'equal))
      (assert-true (find '(:word :|y|) mulm::index-feature-map :test #'equal))
      (assert-true (find '(:word :|13|) mulm::index-feature-map :test #'equal))
      (assert-true (find '(:tag-1 :|0|) mulm::index-feature-map :test #'equal))
      (assert-true (find '(:tag-1 :|1|) mulm::index-feature-map :test #'equal))
      (assert-true (find '(:tag-1 :|<s>|) mulm::index-feature-map :test #'equal))
      (assert-true (equal '(0 1 2)
                          (sort
                           (remove-duplicates (loop for index being the hash-values in mulm::target-index-map
                                                    collect index)
                                              :test #'equal)
                           #'<)))
      (assert-true (find :|0| (loop for target being the hash-keys in mulm::target-index-map
                                    collect target)
                         :test #'equal))
      (assert-true (find :|1| (loop for target being the hash-keys in mulm::target-index-map
                                    collect target)
                         :test #'equal))
      (assert-true (find :|d| (loop for target being the hash-keys in mulm::target-index-map
                                    collect target)
                         :test #'equal))
      (assert-true (= 3 (length mulm::index-target-map)))
      (assert-true (find :|0| mulm::index-target-map :test #'equal))
      (assert-true (find :|1| mulm::index-target-map :test #'equal))
      (assert-true (find :|d| mulm::index-target-map :test #'equal))
      (assert-true (= 3 mulm::c))
      (assert-true (= 6 mulm::p))
      (assert-true (= 18 (length mulm::w)))
      (assert-true (= 0.0 (reduce #'+ mulm::w))))))

(define-test test-features-from-context
  (let* ((sentence (first *corpus1*))
         (context (mapcar #'first sentence))
         (tags (mapcar #'second sentence)))
    (let ((features (mulm::features-from-context *features2* context 2 (elt tags 1) (elt tags 0))))
      (assert-true (= 2 (length features)))
      (assert-true (find '(:tag-1 :|0|) features :test #'equal))
      (assert-true (find '(:word :|x|) features :test #'equal)))))

(define-test test-feature-vector
  (let* ((factor (make-instance 'mulm::feature-factor
                                :features *features2*
                                :cutoff 0
                                :corpus (subseq *corpus1* 0 3)))
         (context (mapcar #'first (first *corpus1*)))
         (result (mulm::feature-vector factor context 2 "0" "1"))
         (expected (sort
                    (list
                     (gethash '(:tag-1 :|0|) (slot-value factor 'mulm::feature-index-map))
                     (gethash '(:word :|x|) (slot-value factor 'mulm::feature-index-map)))
                    #'<)))
    (assert-equal expected result)))

(define-test test-vector-activation
  (let* ((factor (make-instance 'mulm::feature-factor
                                :features *features2*
                                :cutoff 0
                                :corpus (subseq *corpus1* 0 3))))
    ; all zero weight matrix
    (assert-equal '(0.0 0.0 0.0) (mulm::vector-activation factor '(1 4)))

    ; weight matrix with some weights set to 1.0
    (setf (aref (slot-value factor 'mulm::w) 1) 1.0)
    (setf (aref (slot-value factor 'mulm::w) 10) 1.0)
    (setf (aref (slot-value factor 'mulm::w) 13) 1.0)
    (setf (aref (slot-value factor 'mulm::w) 16) 1.0)
    (assert-equal '(1.0 1.0 2.0) (mulm::vector-activation factor '(1 4)))))
