(in-package #:toc-utils)

(defparameter *log-level* 0 "Number representing log level. High means more logging")
(defparameter *log-file* nil "File name to log to.")

(defun format-duration (duration-in-seconds)
  (multiple-value-bind (days seconds)
      (truncate duration-in-seconds 86400)
    (multiple-value-bind (hours secs)
	(truncate seconds 3600)
      (multiple-value-bind (min s)
	  (truncate secs 60)
	(format nil "~a~a~a~a"
		(if (> days 0) (format nil "~d day~:p " days) "")
		(if (> hours 0) (format nil "~d hour~:p " hours) "")
		(if (> min 0) (format nil "~a minute~:p " min) "")
		(if (> s 0) (format nil "~a second~:p" s) ""))))))


(defun print-formatted-time (universal-time)
  (when universal-time
    (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time universal-time)
    (declare (ignore second))
    (format nil "~D:~2,'0D, ~A ~A/~A/~A"
	    hour minute
	    (elt '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day)
	    date month year))))

(defmacro when-bind ((var form) &body body)
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defmacro if-bind ((var form) then-clause else-clause)
  `(let ((,var ,form))
     (if ,var
	 ,then-clause
	 ,else-clause)))

(defclass log-entry ()
  ((log-level :initarg :level :initform 6 :accessor log-level)
    (log-time :initform (get-universal-time) :initarg :time :accessor log-time)
    (log-message :initform "" :initarg :message :accessor log-message)
    (log-function :initform nil :initarg :function :accessor log-function)))

(defun log-equal? (a b)
  (not (some #'null (mapcar (lambda (slot) (equal (slot-value a slot)
					(slot-value b slot)))
		       '(log-level log-time log-message log-function)))))


(defun print-as-unix-log (entries &optional (stream t))
  (dolist (e (sort (copy-list entries) #'< :key #'log-time))
    (format stream "~&~A [~A] ~A ~A~%"
	    (print-formatted-time (log-time e))
	    (nth (log-level e) '("Urgent" "Error" "Warning" "Debug" "Info" "Detailed"))
	    (log-message e) (or (log-function e) ""))))

(defun log-msg (level &rest args)
  "Log in sexp form"
  (when (and *log-file* (<= level *log-level*)) 
    (with-open-file (log *log-file*
			 :direction :output
			 :if-exists :append
			 :if-does-not-exist :create)
      (format log "~&~S~%" `(make-instance 'log-entry
					   :level ,level
					   :message ,(apply #'format nil args)
					   :time ,(get-universal-time))))))

(defparameter *log-entries* nil)

(defun read-log-file (logfile)
  (with-open-file (log logfile)
    (do ((entry (read log nil 'done) (read log nil 'done)))
	((eq entry 'done) (length *log-entries*))
      (let ((obj (eval entry)))
	(pushnew obj *log-entries* :test #'log-equal?)))))

(defun empty-string-p (string)
  (string-equal string ""))


(defmacro define-logged-function (name args &body body) 
  `(defun ,name ,args 
     (write-to-transaction-log (list ',name ,@args)) 
     ,@body))

(defun canonicalize-directory (pathname)
  "Returns a properly formatted directory pathname, if the directory
exists, NIL otherwise."
  (let ((p (probe-file pathname)))
    (when (and p (pathname-directory p) (not (pathname-name p)))
      p)))


(defun make-directory (pathname &optional (octal-mode #o700))
  #+cmu (unix:unix-mkdir (format nil "~A" pathname) octal-mode))


(defun slurp-file-to-string (pathname)
  (with-open-file (f pathname)
    (let ((s (make-string (file-length f))))
      (read-sequence s f)
      s)))

