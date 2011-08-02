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

;; Serialization
;;
;; A lexicon is serialized into a header with an identifier symbol
;; and the lexicon size, followed by the lexicon items line by line.
;;
;; A start and end marker line prepends and appends the lexicon data.
;;
;;
;; Sample:
;;
;; lexicon start
;; lexicon id TEST-LEXICON size 2
;; ba 13
;; foo 666
;; lexicon end

;; string constants used by the serialization format
(defparameter *serialize-lexicon-start-marker* "lexicon start")
(defparameter *serialize-lexicon-end-marker* "lexicon end")

;; Serialization
(defun serialize-lexicon (lexicon s id)
  "Serializes the lexicon to text on the passed stream.
   lexicon - The lexicon instance to be serialized
   s       - A valid stream to write to
   id      - A symbol which will be embedded as a string in the serialization
   Returns the passed stream"
  (format s "~a~%" *serialize-lexicon-start-marker*)
  (format s "lexicon id ~a count ~a~%" id (lexicon-count lexicon))

  (loop for code from 0 below (lexicon-count lexicon)
        do (format s "~a ~a~%" (code-to-token code lexicon) code))

  (format s "~a~%" *serialize-lexicon-end-marker*)

  s)

;; Deserialization
(defun lexicon-parse-header (header)
  "Parses a lexicon header line and returns the data.
   header - header as a string
   Returns a list with the header information, ie. (id size)."
  (let ((tokens (cl-ppcre:all-matches-as-strings "\\S+" header)))
    (unless (equalp (first tokens) "lexicon")
      (error "Lexicon can not be deserialized"))
    (let* ((info (list-to-plist (rest tokens)))
           (id (intern (getf info :id) :keyword))
           (count (parse-integer (getf info :count))))
      (list id count))))

(defun lexicon-parse-line (line)
  "Parses a lexicon data line with token and code separated by whitespace.
   line - a string
   Returns a list with the data, ie. (token code)."
  (let ((tokens (cl-ppcre:all-matches-as-strings "\\S+" line)))
    (destructuring-bind (token code) tokens
      (list token  (parse-integer code)))))

(defun lexicon-insert (lexicon line)
  "Parses a lexicon line and inserts the data into the passed lexicon instance.
   lexicon - instance to be modified
   line    - string with data
   Returns the passed lexicon"
  (destructuring-bind (token code) (lexicon-parse-line line)
    (setf (gethash token (lexicon-forward lexicon)) code)
    (setf (aref (lexicon-backward lexicon) code) token)
    (incf (lexicon-count lexicon))

    lexicon))

(defun deserialize-lexicon (s &optional id)
  "Reads a lexicon from a stream.
   s  - a valid stream
   id - if passed the id read from the stream must correspond to this id
   Returns a list of the lexicon id and the lexicon instance, ie. (id lexicon)."
  (declare (ignore id))
  (unless (equalp (read-line s nil nil)
                  *serialize-lexicon-start-marker*)
    (error "Lexicon can not be deserialized"))

  (let* ((header (read-line s nil nil))
         (info (lexicon-parse-header header)) ;; (id count)
         (id (first info))
         (size (second info))
         (lexicon (make-lexicon :size size)))
    (loop for line = (read-line s nil nil)
          until (equalp (string-trim *whitespace* line) *serialize-lexicon-end-marker*)

          when (null line)
          do (error "Premature end of file")

          do (lexicon-insert lexicon (string-trim *whitespace* line)))
    (list id lexicon)))

(defun lexicon-diff (lex1 lex2)
  (when (not (eql (hash-table-test (lexicon-forward lex1))
                  (hash-table-test (lexicon-forward lex2))))
    (format t "Lexicon test diff~%"))
  (when (/= (lexicon-size lex1) (lexicon-size lex2))
    (format t "Lexicon size diff~%"))
  (when (/= (lexicon-count lex1) (lexicon-count lex2))
    (format t "Lexicon count diff~%"))
  (loop for i from 0 below (min (lexicon-count lex1) (lexicon-count lex2))
        do (when (not (equal (aref (lexicon-backward lex1) i)
                             (aref (lexicon-backward lex2) i)))
             (format t "Lexicon backward diff at ~a: ~a - ~a~%" i
                     (aref (lexicon-backward lex1) i)
                     (aref (lexicon-backward lex2) i))))
  (let ((keys (union (loop for k being the hash-keys of (lexicon-forward lex1) collect k)
                     (loop for k being the hash-keys of (lexicon-forward lex2) collect k))))
    (loop for k in keys
          do (when (/= (gethash k (lexicon-forward lex1))
                       (gethash k (lexicon-forward lex2)))
               (format t "Lexicon forward diff at ~a: ~a - ~a~%" k
                       (gethash k (lexicon-forward lex1))
                       (gethash k (lexicon-forward lex2)))))))
