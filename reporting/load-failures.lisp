;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun load-failures-of (all-results lisp lib-world)
  (subset all-results
          (lambda (result)
            (and (eq :load (car (result-spec result)))
                 (not (eq :ok (caddr (result-spec result))))
                 (search lisp (lisp result))
                 (string= lib-world (lib-world result))))))

(defun as-hash (load-fail-results)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (load-fail load-fail-results)
      (setf (gethash (system-name load-fail) hash)
            t))
    hash))

(defun load-failures-table-rows (load-fail-results)
  (let ((failed-hash (as-hash load-fail-results)))
    (with-output-to-string (s)
      (dolist (fail load-fail-results)
        (let ((system (system-name fail)))
          (format s "<tr><td>~A</td><td>~A</td><td>~A</td><td>~A</td><td>~A</td><td>~A</td></tr>~%"
                  (failure-log-link fail 'system-name)
                  (length (root-blockers system failed-hash))
                  (length (blocked-exclusively system failed-hash))
                  (length (project-names (blocked-exclusively system failed-hash)))
                  (length (project-names (dependents system)))
                  (length (dependents system))))))))

(defun print-load-failures (report-file
                            all-results
                            lisp
                            lib-world)
  (let ((load-fail-results (load-failures-of all-results lisp lib-world)))
    (unless load-fail-results
      (cerror "Build the empty report." "No load failures for the ~A and ~A found." lisp lib-world))
    (with-report-file (out report-file)
      (let ((html-template:*string-modifier* #'cl:identity))
        (html-template:fill-and-print-template (src-file "load-failures-report-template.html")
                                               (list :reports-root-dir-relative-path (reports-root-dir-relative-path report-file)
                                                     :lisp lisp
                                                     :lib-world lib-world
                                                     :table-rows (load-failures-table-rows load-fail-results)
                                                     :time (test-grid-agent::pretty-fmt-time (get-universal-time)))
                                               :stream out)))))
