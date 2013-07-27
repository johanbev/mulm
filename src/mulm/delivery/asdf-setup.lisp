;; Relocatable loader for asdf
;; for use outside the development environment

(in-package :cl-user)

;; fetch the pathname of this file
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *file-path* (directory-namestring (or *compile-file-pathname* *load-pathname*))))

;; root is three levels up
(defparameter *repository-root* (merge-pathnames "../../../" *file-path*))

;; load asdf and register paths in the registry file
(defparameter *asdf-registry-paths-file* (merge-pathnames "../third-party/asdf-central-registries.txt" *file-path*))
#-:asdf
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (compile-file (merge-pathnames "../third-party/asdf.lisp" *file-path*))))

(with-standard-io-syntax
  (let ((*read-eval* nil))
    (with-open-file (s *asdf-registry-paths-file*)
      (loop for path = (read-line s nil nil)
            while path
            unless (eql (elt path 0) #\;)
            do (push (merge-pathnames (string-trim '(#\Space #\Tab #\Newline #\Return) path)
                                      *repository-root*)
                     asdf:*central-registry*)))))
