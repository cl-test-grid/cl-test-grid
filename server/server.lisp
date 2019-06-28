(defpackage :test-grid-server
  (:nicknames :tg-server)
  (:use :cl)
  (:export :start))

(in-package :tg-server)

(defclass tg-acceptor (hunchentoot:easy-acceptor)
  ((smtp-user :type string
              :initarg :smtp-user
              :accessor smtp-user
              :initform (error ":smtp-user is required"))
   (smtp-password :type string
                  :initarg :smtp-password
                  :accessor smtp-password
                  :initform (error ":smtp-password is required"))))

(hunchentoot:define-easy-handler (send-notification :uri "/send-notification")
    (subject body)
  (cl-smtp:send-email "email-smtp.us-east-1.amazonaws.com"
                      "cl.test.grid@gmail.com"
                      "cl-test-grid-notifications@googlegroups.com"
                      subject
                      body
                      :authentication (list :login
                                            (smtp-user hunchentoot:*acceptor*)
                                            (smtp-password hunchentoot:*acceptor*))
                      :ssl t
                      :port 2587))

(defun start (&key port smtp-user smtp-password)
  (hunchentoot:start (make-instance 'tg-acceptor
                                    :port port
                                    :smtp-user smtp-user
                                    :smtp-password smtp-password)))
