(in-package :mime)

(let ((id 0))
  (defun new-id ()
    (incf id))
  (defun reset-id ()
    (setf id 0)))

(defstruct result
  (id (new-id))
  (no-unks 0)
  forms
  gold
  blue
  (length 0)
  (correct 0)
  (unk-correct 0))

(defstruct fold
  (id (new-id))
  results
  (token-length 0)
  (token-correct 0)
  (unknown-tokens 0)
  (unknown-correct 0)
  (known-tokens 0)
  (known-correct 0)
  token-acc
  unk-rat
  unk-acc
  known-acc)


(defun fudge-id (name)
  (sxhash (format nil "~a~a" name (get-internal-real-time))))   
  
(defstruct profile
  name
  id
  folds)

(defun mk-profile (name)
  (make-profile
   :name name
   :id (fudge-id name)))   

(defun register-result (res known-codes)
  (let ((result (make-result)))
    (with-slots (no-unks forms gold blue length correct unk-correct) result
      (setf forms (first res)
            gold (second res)
            blue (third res))
      (loop
          for form in forms
          for gold in gold
          for blue in blue
          do (incf length)
          when (string= gold blue) do
            (incf correct)
          unless (gethash form known-codes) do
            (incf no-unks)
            (when (string= gold blue)
              (incf unk-correct))
          finally (return result)))))

(defun finish-fold (fold)
  (with-slots (token-length token-correct unknown-tokens 
               unknown-correct known-tokens known-correct
               token-acc unk-rat unk-acc known-acc)
      fold
    (setf unk-rat
      (if (= 0 unknown-tokens)
                'N/A
              (* 100
                 (/ unknown-tokens
                    token-length)))
      token-acc
      (* 100 (/ token-correct
                token-length))
      unk-acc
      (if (= 0 unknown-tokens)
                'N/A
              (* 100 
                 (/ unknown-correct
                    unknown-tokens)))
      known-acc
      (if (= 0 known-tokens)
                'N/A
              (* 100
                 (/ known-correct
                    known-tokens))))
    fold))

(defun register-fold (res)
  (let ((fold (make-fold)))
    (with-slots (results token-length token-correct unknown-tokens unknown-correct known-tokens known-correct)
        fold
      (loop
          with known-codes = (first res)
          for seq in (second res)
          for result = (register-result seq known-codes)
          do (with-slots (correct no-unks unk-correct length) result
               (incf token-correct correct)
               (incf token-length length)
               (incf unknown-tokens no-unks)
               (incf unknown-correct unk-correct)
               (incf known-tokens (- length no-unks))
               (incf known-correct (- correct unk-correct)))
             (push result results)
          finally (return (finish-fold fold))))))

(defvar *current-profile* nil)

(defun register-profile (working-set name)
  (let ((profile (mk-profile name)))
    (loop 
        for fold in working-set
        for reg = (register-fold fold)
        do (push reg (profile-folds profile))
        finally (return profile))))

(defun avg-list (list &key (key #'identity))
  (loop
      for x in list
      for i from 1
      sum (funcall key x) into accu
      finally (return (/ accu i))))

(defun std-dev (list &key (key #'identity))
  (loop
      with average = (avg-list list :key key)
      for it in list
      for x = (funcall key it)
      for n from 1
      summing (expt (- x average) 2) into res
      finally (return (sqrt (/ res n)))))
    

(defun print-fold-header (stream)
  (let ((header
         (format nil  "~&~2,8T~a~3,8T~a~4,8T~,2f%~5,8T~a~6,8T~7,8T~,2f~8,8T~,2f%~9,8T~,2f%~%"
                 "Total"
                 "Corr."
                 "ACC"
                 "UNK"
                 "UNK%"
                 "ACC-U"
                 "ACC-K")))
    (format stream header)
    (format stream (make-string (length header) :initial-element #\-))))

(defun print-fold-simply (fold stream)
  (with-slots (token-length token-correct unknown-tokens known-acc unk-acc unk-rat token-acc)
      fold
    (format stream "~&~2,8T~a~3,8T~a~4,8T~,2f%~5,8T~a~6,8T~7,8T~,2f%~8,8T~,2f%~9,8T~,2f%~%"
            token-length
            token-correct
            token-acc
            unknown-tokens
            unk-rat
            unk-acc
            known-acc)))

(defun print-profile-averages (profile stream)
  (let ((folds (profile-folds profile)))
    (format stream "~&~2,8T~a~3,8T~a~4,8T~,2f%~5,8T~a~6,8T~7,8T~,2f%~8,8T~,2f%~9,8T~,2f%~%"
            (float (avg-list folds :key #'fold-token-length))
            (float (avg-list folds :key #'fold-token-correct))
            (avg-list folds :key #'fold-token-acc)
            (float (avg-list folds :key #'fold-unknown-tokens))
            (avg-list folds :key #'fold-unk-rat)
            (avg-list folds :key #'fold-unk-acc)
            (avg-list folds :key #'fold-known-acc))))

(defun print-profile-std-dev (profile stream)
  (let ((folds (profile-folds profile)))
    (format stream "~&~2,8T~,2f~3,8T~,2f~4,8T~,2f%~5,8T~,2f~6,8T~7,8T~,2f%~8,8T~,2f%~9,8T~,2f%~%"
            (float (std-dev folds :key #'fold-token-length))
            (float (std-dev folds :key #'fold-token-correct))
            (std-dev folds :key #'fold-token-acc)
            (float (std-dev folds :key #'fold-unknown-tokens))
            (std-dev folds :key #'fold-unk-rat)
            (std-dev folds :key #'fold-unk-acc)
            (std-dev folds :key #'fold-known-acc))))

(defun print-profile (profile stream)
  (format stream "~%Profile ~a with ~a folds~%" (profile-name profile) (length (profile-folds profile)))
  (print-fold-header stream)
  (loop
      for fold in (profile-folds profile)
      do (print-fold-simply fold stream))
  (format stream "~%")
  (print-fold-header stream)
  (print-profile-averages profile stream)
  (print-profile-std-dev profile stream))
