;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun status-css-class (status)
  (case (aggregated-status status)
    (:ok "ok-status")
    ((:unexpected-ok :known-fail) "warn-status")
    (:fail  "fail-status")
    (:crash  "crash-status")
    (:load-failed  "load-failed-status")
    (:timeout  "timeout-status")
    (:no-resource "no-resource-status")
    (otherwise "")))

(defun single-letter-status (status)
  (let ((aggregated-status status))
    (case aggregated-status
      (:ok "O")
      (:unexpected-ok "U")
      (:fail "F")
      (:crash "C")
      (:load-failed "L")
      (:timeout "T")
      (:known-fail "K")
      (:no-resource "R")
      (otherwise aggregated-status))))

(defun render-single-letter-status (test-run lib-test-result)
  (if (null lib-test-result)
      "&nbsp;"
      (let ((status (aggregated-status (getf lib-test-result :status))))
        (format nil "<a class=\"test-status ~A\" href=\"~A\">~A</a>"
                (status-css-class status)
                (log-uri (make-instance 'joined-lib-result :lib-result lib-test-result :test-run test-run))
                (single-letter-status status)))))

;; todo: this should be a blobstore method, but
;; until we move the reporting to a separate
;; asdf system, we don't want the dependency
;; on blobstore here.
(defun blob-uri (blob-key)
  (format nil "~A/blob?key=~A"
          "http://cl-test-grid.appspot.com" blob-key))

(defparameter *local-test-runs-dir* #P"C:\\Users\\anton\\projects\\cl-test-grid2-work-dir2\\agent\\test-runs\\")

;; todo: won't work on FAILURE objects
(defun lib-log-local-uri (joined-lib-result)
  (format nil "file://~A~A"
          (test-grid-agent::run-directory (test-grid-data::run-descr (test-run joined-lib-result))
                                          *local-test-runs-dir*)
          (string-downcase (libname joined-lib-result))))

(defun no-blob-key-js-alert (&rest unused-args)
  (declare (ignore unused-args))
  "javascript:alert('The blobstore key is not specified, seems like the library log was not submitted to the online storage')")

(defparameter *no-blob-key-printer* (lambda (&rest unused) (break)))
;(defparameter *no-blob-key-printer* 'no-blob-key-js-alert)
;(defparameter *no-blob-key-printer* 'lib-log-local-uri)

(defun log-uri (result)
  (let ((blob-key (log-blob-key result)))
    (if blob-key
        (blob-uri blob-key)
        (funcall *no-blob-key-printer* result))))

(defun log-link (lib-result &rest fields)
  "Generate HTML link to the online test suite log
for the LIB-RESULT. The FIELDS specifies set of fields
to include in to the text of the link, defaults to STATUS"
  (setf fields (or fields '(status)))
  (format nil "<a class=\"~a\" href=\"~a\">~{~a~^, ~}</a>"
          (status-css-class (status lib-result))
          (log-uri lib-result)
          (mapcar (alexandria:rcurry 'funcall lib-result) fields)))

(defun failure-log-link (failure &rest fields)
  "Generate HTML link to the online test suite log
for the RESULT. The FIELDS specifies set of fields
to include in to the text of the link, defaults to STATUS"
  (setf fields (or fields '(fail-spec)))
  (format nil "<a href=\"~a\">~{~a~^, ~}</a>"
          (log-uri failure)
          (mapcar (alexandria:rcurry 'funcall failure) fields)))

;; This is how we print data in table data cells.
(defun format-lib-results (out joined-lib-results)
  (dolist (joined-lib-result joined-lib-results)
    (let ((status (aggregated-status (getf (lib-result joined-lib-result) :status))))
      (format out "<a href=\"~a\" class=\"test-status ~a\">~a</a> "
              (log-uri joined-lib-result)
              (status-css-class status)
              (single-letter-status status)))))

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
