;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(defpackage #:test-grid-reporting/status-icon
  (:nicknames #:tg-reporting/status-icon #:tg-rep/status-icon))

(in-package :tg-reporting/status-icon)

(defun status-icon-input (lib-results lisps lib-worlds)
  "A simplified representation of library results table,
where earch row is a lisp, and each column is a lib-world.
The table cell is a symbol designating testing result.
:FAIL if any failure happend there - a test case, load failure, anything.
:OK if the testing was successful. 
:NONE if there are no results for this lisp / lib-world combination.

Example for three lisps and two lib-worlds:
  ((:ok :fail) (:ok :ok) (:fail :none))
"
  (let ((cells-data (tg-rep::group-by lib-results
                                      '(tg-rep::lisp tg-rep::lib-world))))
    (mapcar (lambda (lisp)
              (mapcar (lambda (lib-world)
                        (let ((cell-data (gethash (list lisp lib-world)
                                                  cells-data)))
                          (cond ((null cell-data) :none)
                                ((some #'tg-rep::failure-p cell-data) :fail)
                                (t :ok))))
                      lib-worlds))
            lisps)))

