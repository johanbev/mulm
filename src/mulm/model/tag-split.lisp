(in-package :mulm)

(defun tag-split (tag counts hmm &optional replacement-table)
  (let* ((total (hash-table-sum counts))
         (types (hash-table-count counts))
         (lexicon (hmm-token-lexicon hmm)))
    (format t "~&Considering tag-splitting `~a' with ~a types and ~a total" tag types total)
    (cond ((> types 50)
           (format t "~&Too many types, tag-split rejected"))
          ((< types 2)
           (format t "~&Too few types, tag-split rejected"))
          (t
           (let* ((coc-table (coc-table counts))
                  (coc-list (coc-list coc-table))
                  (ll (avg-list (mapcar #'second (fudge-smoothing coc-list)))))
             (format t "~&LL number: ~a" ll)
             (if (< ll -1.0)
                 (format t "~& LL is too low for splitting")
               (loop
                   with replacement-table = (or replacement-table (make-hash-table :test #'equal))
                   for form being the hash-keys in counts
                   for count = (gethash form counts)
                   when (> count 75) do
                     (setf (gethash (list form tag) replacement-table)
                       (format nil "~a|~a" (code-to-token form lexicon) tag))
                     (format t "~&Splitted ~a into new tag" (code-to-token form lexicon))
                   finally (return replacement-table))))))))

(defun create-tag-split-table (corpus)
  (let ((hmm (setup-hmm (make-hmm)
                        (corpus-tag-set-size corpus)))
        (replacement-table (make-hash-table :test #'equal)))
    (populate-counts corpus hmm)
    (loop
        for counts across (hmm-emissions hmm)
        for tag across (lexicon-tokens (hmm-tag-lexicon hmm))
        do (tag-split tag counts hmm replacement-table))
    (values replacement-table hmm)))

(defun tag-split-corpora (train test)
  (multiple-value-bind  (replacement-table hmm)
      (create-tag-split-table train)
    (flet ((replacer (corp)
             (loop for sentence in corp
                 collecting
                   (loop 
                       for (form tag) in sentence
                       for code = (token-to-code form (hmm-token-lexicon hmm))
                       for replacement = (gethash (list code tag) replacement-table)
                       if replacement 
                       collect (list form replacement)
                       else collect (list form tag)))))
      (list
       (replacer train)
       (replacer test)))))
