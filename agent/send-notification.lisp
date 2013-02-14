;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; Utility to send notifications to the cl-test-grid-notifications google group:
;;;; https://groups.google.com/forum/?fromgroups=#!forum/cl-test-grid-notifications.
;;;; The messages are send via our Heroku server. That way agent remains
;;;; communicating only via HTTP, and is also restricted to send emails
;;;; only to that particular email adress.

(in-package #:test-grid-agent)

(defun send-notification (subject body &key (server-url "http://cl-test-grid.herokuapp.com/"))
  (handler-case
      (multiple-value-bind (body status)
          (drakma:http-request (format nil "~a/send-notification" server-url)
                               :method :get
                               :parameters (list (cons "subject" subject)
                                                 (cons "body" body)))
        (declare (ignore body))
        (when (/= 200 status)
          (error "Notificateion server returned error HTTP status ~A" status)))
    (serious-condition (c)
      (warn "Failed do send notification due to ~A: ~A" (type-of c) c))))

#|
(send-notification "test, please ignore" "test body")
|#