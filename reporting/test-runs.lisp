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
           (princ (if (char= char #\-)
                      #\|
                      (html-template:escape-string-all (string char)))
                  out)
           (setf maybeBr "<br/>")))))

;; example:
#|
 (string= (vertical-html "cl-abc")
          "c<br/>l<br/>|<br/>a<br/>b<br/>c")
|#

(defun aggregated-status (normalized-status)
  "Returns the test result as one symbol, even
if it was an \"extended status\". Possible return
values: :OK, :UNEXPECTED-OK, :CRASH, :TIMEOUT, :FAIL, :NO-RESOURSE, :KNOWN-FAIL."
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

(defun test-runs-table-html (db &optional (status-renderer 'render-single-letter-status))
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
                (test-grid-agent::pretty-fmt-time (getf run-descr :time))
                (html-template:escape-string-all (princ-to-string (getf run-descr :lib-world)))
                (html-template:escape-string-all (princ-to-string (getf run-descr :lisp)))
                (html-template:escape-string-all (princ-to-string (getf (getf run-descr :contact) :email))))
        (dolist (lib test-grid-testsuites::*all-libs*)
          (format out "<td>~A</td>"
                  (funcall status-renderer run (find lib lib-statuses
                                                     :key (test-grid-utils::plist-getter :libname)))))
        (write-line "</tr>" out)))
    (write-line "</tbody>" out)
    (write-line "</table>" out)))

(defun test-runs-report (db)
  (fmt-template *test-runs-report-template*
                `(("{THE-TABLE}" . ,(test-runs-table-html db))
                  ("{TIME}" . ,(test-grid-agent::pretty-fmt-time (get-universal-time))))))

