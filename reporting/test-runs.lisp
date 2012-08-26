;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defvar *test-runs-report-template*
  (merge-pathnames "test-runs-report-template.html"
                   (src-dir)))

(defun vertical-html (libname)
  (let ((maybeBr "")
        (libname (string libname)))
    (with-output-to-string (out)
      (loop for char across libname
         do (princ maybeBr out)
           (princ (if (char= char #\-) #\| char) out)
           (setf maybeBr "<br/>")))))

;; example:
#|
 (string= (vertical-html "cl-abc")
          "c<br/>l<br/>|<br/>a<br/>b<br/>c")
|#

;; todo: this should be a blobstore method, but
;; until we move the reporting to a separate
;; asdf system, we don't want the dependency
;; on blobstore here.
(defun blob-uri (blob-key)
  (format nil "~A/blob?key=~A"
          "http://cl-test-grid.appspot.com" blob-key))

(defparameter *local-test-runs-dir* #P"C:\\Users\\anton\\projects\\cl-test-grid2-work-dir2\\agent\\test-runs\\")

(defun lib-log-local-uri (joined-lib-result)
  (format nil "file://~A~A"
          (test-grid-agent::run-directory (test-grid-data::run-descr (test-run joined-lib-result))
                                          *local-test-runs-dir*)
          (string-downcase (getf (lib-result joined-lib-result) :libname))))

(defun no-blob-key-js-alert (&rest unused-args)
  (declare (ignore unused-args))
  "javascript:alert('The blobstore key is not specified, seems like the library log was not submitted to the online storage')")

;(defparameter *no-blob-key-printer* 'no-blob-key-js-alert)
(defparameter *no-blob-key-printer* 'lib-log-local-uri)

(defun lib-log-uri (joined-lib-result)
  (let ((blob-key (getf (lib-result joined-lib-result) :log-blob-key)))
    (if blob-key
        (blob-uri blob-key)
        (funcall *no-blob-key-printer* joined-lib-result))))

(defun aggregated-status (normalized-status)
  "Returns the test result as one symbol, even
if it was an \"extended status\". Possible return
values: :OK, :UNEXPECTED-OK, :CRASH, :TIMEOUT, :LOAD-FAILED, :FAIL, :NO-RESOURSE, :KNOWN-FAIL."
  (etypecase normalized-status
    (symbol normalized-status)
    (list (destructuring-bind (&key failed-tests known-to-fail) normalized-status
            (cond ((null failed-tests)
                   (if (null known-to-fail)
                       :ok
                       :unexpected-ok))
                  ((test-grid-utils::set= failed-tests known-to-fail :test #'string=)
                   :known-fail)
                  (t :fail))))))

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

(defun render-single-letter-status (test-run lib-test-result)
  (if (null lib-test-result)
      "&nbsp;"
      (let ((status (aggregated-status (getf lib-test-result :status))))
        (format nil "<a class=\"test-status ~A\" href=\"~A\">~A</a>"
                (status-css-class status)
                (lib-log-uri (make-instance 'joined-lib-result :lib-result lib-test-result :test-run test-run))
                (single-letter-status status)))))

(defun test-runs-table-html (&optional
                             (db test-grid-data::*db*)
                             (status-renderer 'render-single-letter-status))
  (with-output-to-string (out)
    (write-line "<table cellspacing=\"1\" class=\"tablesorter\">" out)

    (princ "<thead><tr style=\"vertical-align: bottom;\"><th>Start Time</th><th>Lib World</th><th>Lisp</th><th>Runner</th>" out)
    (dolist (lib test-grid-testsuites::*all-libs*)
      (format out "<th>~A</th>" (vertical-html lib)))
    (write-line "</tr></thead>" out)

    (write-line "<tbody>" out)
    (dolist (run (getf db :runs))
      (let ((run-descr (test-grid-data::run-descr run))
            (lib-statuses (test-grid-data::run-results run)))
        (format out "<tr><td>~A</td><td>~A</td><td>~A</td><td>~A</td>"
                (test-grid-testsuites::pretty-fmt-time (getf run-descr :time))
                (getf run-descr :lib-world)
                (getf run-descr :lisp)
                (getf (getf run-descr :contact) :email))
        (dolist (lib test-grid-testsuites::*all-libs*)
          (format out "<td>~A</td>"
                  (funcall status-renderer run (find lib lib-statuses
                                                     :key (test-grid-utils::getter :libname)))))
        (write-line "</tr>" out)))
    (write-line "</tbody>" out)
    (write-line "</table>" out)))

(defun test-runs-report (&optional (db test-grid-testsuites::*db*))
  (fmt-template *test-runs-report-template*
                `(("{THE-TABLE}" . ,(test-runs-table-html db))
                  ("{TIME}" . ,(test-grid-testsuites::pretty-fmt-time (get-universal-time))))))

