(in-package :mulm)

;; The lexicon holds the tokens of a corpora and their mapping
;; to internal codes
(defstruct (lexicon
            ;; This constructor is for internal use only
            (:constructor init-lexicon
             (&key (test #'equal)
                   (forward (make-hash-table :test test))
                   (backward (make-array 512))
                   (size 512)
                   (count 0))))
  forward backward size count)

;; external constructor for lexicon struct
(defun make-lexicon (&rest kwords &key &allow-other-keys)
  (let ((size (getf kwords :size))
        (lexicon (apply #'init-lexicon kwords)))
    (when size
      (setf (lexicon-backward lexicon)
            (adjust-array (lexicon-backward lexicon) size)))
    
    lexicon))

(defmethod print-object ((object lexicon) stream)
  (let ((n (hash-table-count (lexicon-forward object))))
    (format
     stream 
     "#<Lexicon (~d forward~p, ~d backward~p of ~s)>"
     n n
     (lexicon-count object)
     (lexicon-count object)
     (lexicon-size object))))

(defun token-to-code (token lexicon &key rop)
  "Translate token to code, if the token is added if it's not already in the lexicon.
   rop - read-only, new tokens are not added
   returns the integer code for the token"
  (or
   ;; we have the token already, return the code
   (gethash token (lexicon-forward lexicon))
   ;; add the token unless read-only
   (unless rop
     (let* ((i (lexicon-count lexicon)))
       ;; add to forwards map
       (setf (gethash token (lexicon-forward lexicon)) i)
       (when (>= i (lexicon-size lexicon))
         ;; expand backwards array if it's full
         (setf (lexicon-size lexicon) (* 2 (lexicon-size lexicon)))
         (setf (lexicon-backward lexicon)
               (adjust-array 
                (lexicon-backward lexicon) (lexicon-size lexicon))))
       ;; add to backwards map
       (setf (aref (lexicon-backward lexicon) i) token)

       ;; codes start at zero so increment count after using it for the new code
       (incf (lexicon-count lexicon))
       
       i))))

(defun code-to-token (code lexicon)
  "Translates an integer code to the corresponding token.
   returns the integer code or nil if the token is not present in the lexicon."
  (when (< code (lexicon-count lexicon))
    (aref (lexicon-backward lexicon) code)))

(defun lexicon-tokens (lexicon)
  "Returns an array with the tokens in the lexicon."
  (lexicon-backward lexicon))

(defun serialize-lexicon (lexicon s id)
  (format s "lexicon start")
  (format s "lexicon id ~a count ~a~%" id (lexicon-count lexicon))

  (loop for code from 0 below (lexicon-count lexicon)
        do (format s "lexicon ~a ~a~%" (code-to-token code lexicon) code))

  (format s "lexicon end~%")

  s)

(defun list-to-plist (list)
  (loop for elt in list
        for i from 1
        collect (if (oddp i)
                  (intern (string-upcase elt) :keyword)
                  elt)))

(defun lexicon-parse-header (header)
  (let ((tokens (cl-ppcre:all-matches-as-strings "\\S+" header)))
    (unless (equalp (first tokens) "lexicon")
      (error "Lexicon can not be deserialized"))
    (let* ((info (list-to-plist (rest tokens)))
           (id (getf info :id))
           (count (parse-integer (getf info :count))))
      (list id count))))

(defun lexicon-parse-line (line)
  (let ((tokens (cl-ppcre:all-matches-as-strings "\\S+" line)))
    (unless (equalp (first tokens) "lexicon")
      (error "Lexicon can not be deserialized"))
    (destructuring-bind (token code) (rest tokens)
      (list token  (parse-integer code)))))

(defun lexicon-insert (lexicon line)
  (destructuring-bind (token code) (lexicon-parse-line line)
    (setf (gethash token (lexicon-forward lexicon)) code)
    (setf (aref (lexicon-backward lexicon) code) token)
    (incf (lexicon-count lexicon))

    lexicon))

(defun deserialize-lexicon (s &optional id)
  (declare (ignore id))

  (unless (equalp (read-line s nil nil)
                  "lexicon start")
    (error "Lexicon can not be deserialized"))

  (let* ((header (read-line s nil nil))
         (info (lexicon-parse-header header)) ;; (id count)
         (lexicon (make-lexicon :size (second info))))
    (loop for line = (read-line s nil nil)
          until (equalp (string-trim *whitespace* line) "lexicon end")

          when (null line)
          do (error "Premature end of file")

          do (lexicon-insert lexicon (string-trim *whitespace* line)))
    lexicon))
