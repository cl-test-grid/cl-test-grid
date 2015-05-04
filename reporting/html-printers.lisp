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
                (html-template:escape-string-all (log-uri (make-instance 'joined-lib-result
                                                                         :lib-result lib-test-result
                                                                         :test-run test-run)))
                (html-template:escape-string-all (single-letter-status status))))))

;; todo: this should be a blobstore method, but
;; until we move the reporting to a separate
;; asdf system, we don't want the dependency
;; on blobstore here.
(defun blob-uri (blob-key)
  (unless (every (lambda (char)
                   (find char "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                 blob-key)
    (error "BLOB-KEY must only consist of digits or letters to prevent URI injection (as we haven't implemented URL-encoding): ~A" blob-key))
  (format nil "~A/blob?key=~A"
          "http://cl-test-grid.appspot.com" blob-key))

(defparameter *local-test-runs-dir* #P"/Users/anton/projects/cl-test-grid2-work-dir2/agent/test-runs/")

;; todo: won't work on FAILURE objects
(defun lib-log-local-uri (joined-lib-result)
  (format nil "file://~A~A"
          (test-grid-agent::run-directory (test-grid-data::run-descr (test-run joined-lib-result))
                                          *local-test-runs-dir*)
          (html-template:escape-string-all (string-downcase (libname joined-lib-result)))))

(defun no-blob-key-js-alert (&rest unused-args)
  (declare (ignore unused-args))
  "javascript:alert('The blobstore key is not specified, seems like the library log was not submitted to the online storage')")

(defparameter *no-blob-key-printer* (lambda (result)
                                      (cerror "Result ~S has no blob key"
                                              (list-props result (list #'lisp #'libname #'lib-world)))))
;(defparameter *no-blob-key-printer* 'no-blob-key-js-alert)
;(defparameter *no-blob-key-printer* 'lib-log-local-uri)

(defun log-uri (result)
  (let ((blob-key (log-blob-key result)))
    (if blob-key
        (blob-uri blob-key)
        (funcall *no-blob-key-printer* result))))

(defun fields-to-string (object fields)
  (format nil "~{~a~^, ~}" (list-props object fields)))

(defun log-link (lib-result &rest fields)
  "Generate HTML link to the online test suite log
for the LIB-RESULT. The FIELDS specifies set of fields
to include in to the text of the link, defaults to STATUS"
  (setf fields (or fields '(status)))
  (format nil "<a class=\"~a\" href=\"~a\">~a</a>"
          (status-css-class (status lib-result))
          (html-template:escape-string-all (log-uri lib-result))
          (html-template:escape-string-all (fields-to-string lib-result fields))))

(defun failure-log-link (failure &rest fields)
  "Generate HTML link to the online test suite log
for the RESULT. The FIELDS specifies set of fields
to include in to the text of the link, defaults to FAIL-SPEC"
  (setf fields (or fields '(fail-spec)))
  (format nil "<a href=\"~a\">~a</a>"
          (html-template:escape-string-all (log-uri failure))
          (html-template:escape-string-all (fields-to-string failure fields))))

(defun result-css-class (result)
  (let ((status (ecase (first (result-spec result))
                  (:whole-test-suite (second (result-spec result)))
                  ((:test-case :load) (third (result-spec result))))))
    (ecase status
      (:ok "ok-status")
      ((:known-fail :unexpected-ok) "warn-status")
      (:fail  "fail-status")
      (:crash  "crash-status")
      (:timeout  "timeout-status")
      (:no-resource "no-resource-status"))))

(defun result-log-link (result &rest fields)
  "Generate HTML link to the online test suite log
for the RESULT. The FIELDS specifies set of fields
to include in to the text of the link, defaults to RESULT-SPEC"
  (setf fields (or fields '(result-spec)))
  (format nil "<a class=\"~a\" href=\"~a\">~a</a>"
          (result-css-class result)
          (html-template:escape-string-all (log-uri result))
          (html-template:escape-string-all (fields-to-string result fields))))

(defun note-html (note)
  (etypecase note
    (github-issue (format nil "<a class=\"note\" href=\"~A\">#~A/~A</a>" (ticket-url note) (repo note) (numbr note)))
    (launchpad-ticket (format nil "<a class=\"note\" href=\"~A\">#lp~A</a>" (ticket-url note) (id note)))
    (prj-ticket (format nil "<a class=\"note\" href=\"~A\">#~(~A~)/~A</a>" (ticket-url note) (project-key note) (ticket-id note)))
    (string (format nil "<span class=\"note\">~A</span>" (html-template:escape-string-all note)))))

(defun notes-html (result)
  (format nil "~{~A~^, ~}"
          (mapcar #'note-html (notes result))))

(defun results-cell-printer (out cell-data &rest fields)
  "Convenient for the most cases printer of
a RESULT object list for a pivot table cell.
CELL-DATA is the list of RESULT objects to print."
  (dolist (result (sort (copy-list cell-data)
                        #'string<
                        :key (lambda (elem) (fields-to-string elem fields))))
    (format out "<div class=\"result\">~A ~A</div>"
            (apply #'result-log-link result fields)
            (notes-html result))))

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
