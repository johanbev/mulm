(in-package :cl-user)

;; simple top level driver for command line delivery
(defun top-level-tag (model-file in-file)
  (let* ((model (mulm::deserialize-hmm-model-from-file model-file))
         (decoder (mulm::make-decoder-from-model model
                                                 (mulm::make-description :order 2
                                                                         :smoothing :deleted-interpolation))))

    (log5:log-for (log5:info) "Started tagging corpus")

    (mulm::read-tt-corpus in-file
                          :sentence-handler #'(lambda (sentence)
                                                (loop for (token tag) in (mulm::process-sentence sentence decoder)
                                                      do (format t "~a~C~a~%" token #\Tab tag))
                                                (format t "~%"))
                          :collect nil)

    (log5:log-for (log5:info) "Completed tagging corpus")))

(defun top-level-train (in-file &optional model-file)
  (log5:log-for (log5:info) "Training HMM model")
  (let ((model (mulm::train (mulm::read-tt-corpus in-file))))
    (mulm::serialize-hmm-model-to-file model (or model-file (concatenate 'string in-file ".mulm")))))

(defun top-level-print-help ()
  (format t 
          "Usage: mulm -model model-file tag input-file OR
            [-model model-file] train corpus-file~%"))

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

    (when (or (null args) 
              (> (length args) 2)
              (not (null errs)))
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
        (:train (top-level-train *input-file* *model-file*))
        (:help (top-level-print-help))
        (t (top-level-print-help))))

    (when *verbose*
      (format *error-output* "~%"))))

(defun main ()
  ;; just print help if called w/o args
  (if (null (mulm::get-command-line-args))
      (top-level-print-help)
    (handler-case (main-body)
      (argument-error nil (top-level-print-help)))))
