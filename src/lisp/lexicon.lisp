(in-package :mulm)

;; These tokens are added to every lexicon
(defparameter *default-tokens*
  ;; model internal sequence start/end tokens
  '("<s>" "</s>"))

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
  (let ((lexicon (apply #'init-lexicon kwords)))
    ;; inject default tokens
    (loop for token in *default-tokens*
        do (token-to-code token lexicon))
    
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
