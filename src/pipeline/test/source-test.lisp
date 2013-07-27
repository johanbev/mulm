(in-package :mulm-test)

(defparameter *conll-input-1*
  (mulm::tabbed-string '(("ba" "foo")
                         ("knark" "bork")
                         ()
                         ("frob" "fnib"))))

(defparameter *conll-input-2*
  (mulm::tabbed-string '(("Year" "year" "nn")
                         ("2000" "2000" "num")
                         ("." "." ".")
                         ()
                         ("Happy" "happy" "jj")
                         ("new" "new" "jj")
                         ("year" "year" "nn")
                         ("!" "!" "punct"))))

(defun get-word (sentences sent-index word-index)
  (elt (rest (assoc :words (elt sentences sent-index))) word-index))

(deftestsuite source (all)
  (source-1 sentences-1 source-2 sentences-2)
  (:setup
   (setf source-1 (make-instance 'mulm::conll-source
                               :str (make-string-input-stream *conll-input-1*)))
   (setf sentences-1 (mulm::all-sentences source-1))

   (setf source-2 (make-instance 'mulm::conll-source
                                 :str (make-string-input-stream *conll-input-2*)
                                 :columns '(:form :lemma :pos)))
   (setf sentences-2 (mulm::all-sentences source-2)))
  
  (:test
   (basic
    (ensure (listp sentences-1))
    (ensure-same (length sentences-1) 2)
    
    (ensure-same (rest (assoc :index (first sentences-1))) 0)
    (ensure-same (rest (assoc :index (second sentences-1))) 1)

    (ensure-same (length (rest (assoc :words (first sentences-1)))) 2)
    (ensure-same (length (rest (assoc :words (second sentences-1)))) 1)
    (ensure-same (rest (assoc :form (get-word sentences-1 0 0))) "ba")
    (ensure-same (rest (assoc :pos (get-word sentences-1 0 0))) "foo")
    (ensure-same (rest (assoc :form (get-word sentences-1 0 1))) "knark")
    (ensure-same (rest (assoc :pos (get-word sentences-1 0 1))) "bork")
    (ensure-same (rest (assoc :form (get-word sentences-1 1 0))) "frob")
    (ensure-same (rest (assoc :pos (get-word sentences-1 1 0))) "fnib")))
  (:test
   (columns
    (ensure (listp sentences-2))
    (ensure-same (length sentences-2) 2)

    (ensure-same (rest (assoc :index (first sentences-2))) 0)
    (ensure-same (rest (assoc :index (second sentences-2))) 1)

    (ensure-same (length (rest (assoc :words (first sentences-2)))) 3)
    (ensure-same (length (rest (assoc :words (second sentences-2)))) 4)

    (ensure-same (rest (assoc :form (get-word sentences-2 0 0))) "Year")
    (ensure-same (rest (assoc :lemma (get-word sentences-2 0 0))) "year")
    (ensure-same (rest (assoc :pos (get-word sentences-2 0 0))) "nn")
    (ensure-same (rest (assoc :form (get-word sentences-2 0 1))) "2000")
    (ensure-same (rest (assoc :lemma (get-word sentences-2 0 1))) "2000")
    (ensure-same (rest (assoc :pos (get-word sentences-2 0 1))) "num")
    (ensure-same (rest (assoc :form (get-word sentences-2 0 2))) ".")
    (ensure-same (rest (assoc :lemma (get-word sentences-2 0 2))) ".")
    (ensure-same (rest (assoc :pos (get-word sentences-2 0 2))) ".")
    
    (ensure-same (rest (assoc :form (get-word sentences-2 1 0))) "Happy")
    (ensure-same (rest (assoc :lemma (get-word sentences-2 1 0))) "happy")
    (ensure-same (rest (assoc :pos (get-word sentences-2 1 0))) "jj")
    (ensure-same (rest (assoc :form (get-word sentences-2 1 1))) "new")
    (ensure-same (rest (assoc :lemma (get-word sentences-2 1 1))) "new")
    (ensure-same (rest (assoc :pos (get-word sentences-2 1 1))) "jj")
    (ensure-same (rest (assoc :form (get-word sentences-2 1 2))) "year")
    (ensure-same (rest (assoc :lemma (get-word sentences-2 1 2))) "year")
    (ensure-same (rest (assoc :pos (get-word sentences-2 1 2))) "nn")
    (ensure-same (rest (assoc :form (get-word sentences-2 1 3))) "!")
    (ensure-same (rest (assoc :lemma (get-word sentences-2 1 3))) "!")
    (ensure-same (rest (assoc :pos (get-word sentences-2 1  3))) "punct")))
     
  (:teardown
   (close source-1)
   (close source-2)))

