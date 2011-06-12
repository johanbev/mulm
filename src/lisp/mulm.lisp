(in-package :cl-user)

(defpackage :mulm  (:use "COMMON-LISP"))

(in-package :mulm)

;; store the root path of the lisp code directory
(defparameter *package-path*
  (directory-namestring
   (asdf:component-pathname (asdf:find-system :mulm))))

;; default placement of evaluation corpora is beside the src directory
(defparameter *eval-path*
  (merge-pathnames "../../eval/" *package-path*))

;; POS tagging evaluation corpus
(defparameter *tagger-train-file*
  (merge-pathnames "wsj.tt" *eval-path*))

(defparameter *tagger-eval-file*
  (merge-pathnames "test.tt" *eval-path*))
