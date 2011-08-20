(in-package :mulm)

(defun verbose-logging ()
  (log5:stop-sender 'quiet :warn-if-not-found-p nil)
  (log5:start-sender 'verbose
                     (log5:stream-sender :location *error-output*)
                     :category-spec '(log5:trace+)
                     :output-spec '(log5:category
                                    log5:message)))

(defun quiet-logging ()
  (log5:stop-sender 'verbose :warn-if-not-found-p nil)
  (log5:start-sender 'quiet
                     (log5:stream-sender :location *error-output*)
                     :category-spec '(log5:warn+)
                     :output-spec '(log5:category
                                    log5:message)))

(defvar *log-sender* (quiet-logging))
