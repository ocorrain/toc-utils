(in-package #:cl-user)

(defpackage #:toc-utils
  (:use #:cl
   #+cmu #:pcl
   #+openmcl #:ccl)
  (:export
   #:canonicalize-directory
   #:make-directory
   #:*log-level*
   #:*log-entries*
   #:read-log-file
   #:log-entry
   #:*log-file*
   #:format-duration
   #:print-formatted-time
   #:when-bind
   #:if-bind
   #:log-msg
   #:define-logged-function
   #:empty-string-p))
