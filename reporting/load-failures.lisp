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
  (let ((failed-hash (as-hash failures)))
    (with-output-to-string (s)
      (dolist (fail failures)
        (let ((system (system-name fail)))
          (format s "<tr><td>~A</td><td>~A</td><td>~A</td><td>~A</td><td>~A</td><td>~A</td></tr>~%"
                  (failure-log-link fail 'system-name)
                  (length (root-blockers system failed-hash))
                  (length (blocked-exclusively system failed-hash))
                  (length (project-names (blocked-exclusively system failed-hash)))
                  (length (project-names (dependents system)))
                  (length (dependents system))))))))

(defun print-load-failures (report-file
                            all-failures
                            lisp
                            lib-world)
  (let ((load-failures (load-failures-of all-failures lisp lib-world)))
    (with-report-file (out report-file)
      (let ((html-template:*string-modifier* #'cl:identity))
        (html-template:fill-and-print-template (src-file "load-failures-report-template.html")
                                               (list :reports-root-dir-relative-path (reports-root-dir-relative-path report-file)
                                                     :lisp lisp
                                                     :lib-world lib-world
                                                     :table-rows (load-failures-table-rows load-failures)
                                                     :time (test-grid-agent::pretty-fmt-time (get-universal-time)))
                                               :stream out)))))
