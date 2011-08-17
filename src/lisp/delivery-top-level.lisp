(in-package :cl-user)

;; simple top level driver for command line delivery
(defun top-level-tag (model-file in-file)
  (let ((model (mulm::deserialize-hmm-model-from-file model-file))
        (input (mulm::read-tt-corpus in-file)))
    (mulm::make-transition-table model 2 :deleted-interpolation)
    (loop for sentence in input
          for tokens = (mapcar #'first sentence)
          for result = (mulm::viterbi-trigram model tokens)
          do (loop for tok in tokens
                   for tag in result
                   do (format t "~a~C~a~%" tok #\Tab tag))
          do (format t "~%"))))

(defun top-level-train (in-file)
  (let ((model (mulm::train (mulm::read-tt-corpus in-file))))
    (mulm::serialize-hmm-model-to-file model (concatenate 'string in-file ".mulm"))))

(defun top-level-print-help ()
  (format t "Help!~%"))

(defparameter *opt-specifier* '(("verbose" :none)
                                ("model" :required)))

(defparameter *verbose* nil)
(defparameter *model-file* nil)
(defparameter *input-file* nil)

(define-condition argument-error (error)
  ((text :initarg :text :reader text)))

(defun main-body ()
  (multiple-value-bind (args opts errs)
      (getopt:getopt (mulm::get-command-line-args) *opt-specifier*)

    (when (or (null args) (> (length args) 2) (not (null errs)))
      (error 'argument-error :text "malformed argument list"))

    (when (= (length args) 2)
      (setf *input-file* (second args)))
    
    (loop for opt in opts
          do (cond ((equal "verbose" (first opt))
                    (setf *verbose* t)
                    (mulm::verbose-logging))
                   ((equal "model" (first opt))
                    (setf *model-file* (cdr opt)))))

    (let ((action (mulm::make-keyword (first args))))
      (case action
        (:tag (top-level-tag *model-file* *input-file*))
        (:train (top-level-train *input-file*))
        (:help (top-level-print-help))))))

(defun main ()
  (handler-case (main-body)
    (argument-error (top-level-print-help) nil)))
