;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun load-failures-of (all-failures lisp lib-world)
  (remove-if-not (lambda (fail)
                   (and (eq :load (car (fail-spec fail)))
                        (search lisp (lisp fail))
                        (string= lib-world (lib-world fail))))
                 all-failures))

(defun as-hash (load-failures)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (load-fail load-failures)
      (setf (gethash (system-name load-fail) hash)
            t))
    hash))

(defun load-failures-table-rows (failures)
  (with-output-to-string (s)
    ;; he-he, modifying global variable (to be refactored
    (set-failed-systems (as-hash failures))
    (dolist (fail failures)
      (let ((system (system-name fail)))
        (format s "<tr><td>~A</td><td>~A</td><td>~A</td><td>~A</td><td>~A</td><td>~A</td></tr>~%"
                (failure-log-link fail 'system-name)
                (length (root-blockers system))
                (length (blocked-exclusively system))
                (length (project-names (blocked-exclusively system)))
                (length (project-names (dependents system)))
                (length (dependents system)))))))

(defun print-load-failures (all-failures lisp lib-world
                            report-file-name)
  (let ((load-failures (load-failures-of all-failures lisp lib-world)))
    (with-report-file (out report-file-name)
      (write-sequence (fmt-template (src-file "load-failures-report-template.html")
                                    `(("{LISP}" . ,lisp)
                                      ("{LIB-WORLD}" . ,lib-world)
                                      ("{TABLE-ROWS}" . ,(load-failures-table-rows load-failures))
                                      ("{TIME}" . ,(test-grid-agent::pretty-fmt-time (get-universal-time)))))
                      out))
    )
  nil)

#|

;; Usage:

(defparameter *db* (test-grid-data:read-db))
(defparameter *results* (test-grid-reporting::select *db*))
(defparameter *failures* (test-grid-reporting::list-failures *results*))

(print-load-failures *failures*
                     "ecl-12.7.1-dfc94901-linux-x86-lisp-to-c"
                     "quicklisp 2012-09-09"
                     "ecl-load-failures.html")
|#