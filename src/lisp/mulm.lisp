(in-package :cl-user)

(defpackage :mulm
  (:use "COMMON-LISP")
  (:import-from :split-sequence split-sequence split-sequence-if))

(in-package :mulm)

#+:allegro(declaim (optimize (debug 2)))

;; store the root path of the lisp code directory
(defparameter *package-path*
  (directory-namestring
   (asdf:component-pathname (asdf:find-system :mulm))))

;; default placement of evaluation corpora is beside the src directory
(defparameter *eval-path*
  (merge-pathnames "../../eval/" *package-path*))

(defparameter *whitespace* '(#\Tab #\Space #\Newline))

(defparameter *start-tag* "<s>")
(defparameter *end-tag* "</s>")

(defvar *hmm* nil)
