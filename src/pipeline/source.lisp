(in-package :mulm)

(defclass conll-source ()
  ((id             :initarg :id        :initform :unknown-conll-source)
   (str            :initarg :str       :initform (error "Must supply a source stream"))
   (processor      :initarg :processor :initform nil)
   (line-number    :initform 0)
   (sentence-count :initform 0)))

(defgeneric next-line (source &key))
(defgeneric next-sentence (source &key))
(defgeneric reset (source &key))
(defgeneric close (source &key))
(defgeneric all-sentences (source &key))

(defmethod next-line ((source conll-source) &key)
  (with-slots (str line-number) source
    (let ((line (read-line str nil nil)))
      (incf line-number)
      (and line (string-trim '(#\Space #\Tab #\Newline) line)))))

(defun parse-word (line)
  (let ((tokens (split-sequence:split-sequence #\Tab line)))
    (when (/= (length tokens) 2)
      (error "Parse error"))
    (list (cons :form (first tokens))
          (cons :pos (second tokens)))))

(defmethod next-sentence ((source conll-source) &key)
  (with-slots (sentence-count line-number) source
    (loop for line = (next-line source)
          while (and line (> (length line) 0))
          with words = nil
          for word = (parse-word line)
          do (push (cons :line-num line-number) word)
          do (push word words)
          finally (return
                   (if words
                     (let ((sentence (list (cons :index sentence-count)
                                           (cons :words (nreverse words)))))
                       (incf sentence-count)
                       sentence)
                     nil)))))

(defmethod reset ((source conll-source) &key)
  (with-slots (line-number sentence-count str) source
    (setf line-number 0)
    (setf sentence-count 0)
    (file-position str 0)

    source))

(defmethod close ((source conll-source) &key)
  (with-slots (str) source
    (close str)))

(defmethod all-sentences ((source conll-source) &key)
  (loop for sentence = (next-sentence source)
        while sentence
        collect sentence))
