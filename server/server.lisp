(defpackage :test-grid-server
  (:nicknames :tg-server)
  (:use :cl)
  (:export :start))

(in-package :tg-server)

(defclass tg-acceptor (hunchentoot:easy-acceptor)
  ((smtp-password :type string
                  :initarg :smtp-password
                  :accessor smtp-password
                  :initform (error ":smtp-password is required"))))

(hunchentoot:define-easy-handler (send-notification :uri "/send-notification")
    (subject body)
  (cl-smtp:send-email "smtp.gmail.com"
                      "cl.test.grid@gmail.com"
                      "cl-test-grid-notifications@googlegroups.com"
                      subject
                      body
                      :authentication (list :login
                                            "cl.test.grid@gmail.com"
                                            (smtp-password hunchentoot:*acceptor*))
                      :ssl t))

(defun start (&key port smtp-password)
  (hunchentoot:start (make-instance 'tg-acceptor
                                    :port port
                                    :smtp-password smtp-password)))