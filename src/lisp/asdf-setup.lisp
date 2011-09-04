;; Relocatable loader for asdf
;; for use outside the development environment
;;
;; NOTE this file cannot be compiled by itself, use load()

(in-package :cl-user)

;; fetch the pathname of this file
(defparameter *file-path* (or *compile-file-pathname* *load-pathname*))

;; root is two levels up
(defparameter *repository-root* (merge-pathnames "../../" *file-path*))

;; load asdf and register paths in the registry file
(defparameter *asdf-registry-paths-file* (merge-pathnames "third-party/asdf-central-registries.txt" *file-path*))
#-:asdf
(load (compile-file (merge-pathnames "third-party/asdf.lisp" *file-path*)))

(with-standard-io-syntax
  (let ((*read-eval* nil))
    (with-open-file (s *asdf-registry-paths-file*)
      (loop for path = (read-line s nil nil)
            while path
            unless (eql (elt path 0) #\;)
            do (push (merge-pathnames (string-trim '(#\Space #\Tab #\Newline #\Return) path)
                                      *repository-root*)
                     asdf:*central-registry*)))))
