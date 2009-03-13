; -*- Mode: Lisp; -*-

(in-package #:cl-user)

(defpackage #:toc-utils-system
  (:use #:cl
	#:asdf))

(in-package #:toc-utils-system)

(defsystem toc-utils
  :components ((:file "package")
	       (:file "utils" :depends-on ("package"))))






