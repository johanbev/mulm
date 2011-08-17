(in-package :mulm)

(defun verbose-logging ()
  (log5:stop-sender 'quiet)
  (log5:start-sender 'verbose
                     (log5:stream-sender :location *error-output*)
                     :category-spec '(log5:trace+)
                     :output-spec '(log5:time
                                    log5:category
                                    log5:message
                                    log5:context)))

(defun quiet-logging ()
  (log5:stop-sender 'verbose)
  (log5:start-sender 'quiet
                     (log5:stream-sender :location *error-output*)
                     :category-spec '(log5:warn+)
                     :output-spec '(log5:time
                                    log5:category
                                    log5:message)))

(defvar *log-sender* (quiet-logging))
