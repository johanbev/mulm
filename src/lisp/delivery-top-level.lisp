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

(defun main ()
  (let* ((command (mulm::parse-command-line-arguments (mulm::get-command-line)))
         (action (first command)))
    (case action
      (:tag (top-level-tag (second command) (third command)))
      (:train (top-level-train (second command)))
      ((:help :unknown) (top-level-print-help)))))
