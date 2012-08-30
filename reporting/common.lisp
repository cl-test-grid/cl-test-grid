;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun log-link (result &rest fields)
  "Generate HTML link to the online test suite log
for the RESULT. The FIELDS specifies set of fields
to include in to the text of the link, defaults to STATUS"
  (setf fields (or fields '(status)))
  (format nil "<a class=\"~a\" href=\"~a\">~{~a~^, ~}</a>"
          (status-css-class (status result))
          (lib-log-uri result)
          (mapcar (alexandria:rcurry 'funcall result) fields)))

(defun report-page (out title body)
  (format out "<html><head>~%")
  (format out "  <title>~A - CL Test Grid</title>~%" title)
  (format out "  <link href=\"style.css\" rel=\"stylesheet\"/><head>~%")
  (format out "</head>~%")
  (format out "<body>~%")
  (format out "~A~%" body)
  (format out "<p class=\"timestamp\">Generated at: ~A GMT.</p>"
          (test-grid-agent::pretty-fmt-time (get-universal-time)))
  (format out "</body>~%")
  (format out "</html>~%"))
