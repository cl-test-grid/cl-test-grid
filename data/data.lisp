;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(defpackage #:test-grid-data
  (:use :cl))

(in-package #:test-grid-data)

(defparameter *db* '(:version 0 :runs ()))

(defun src-dir()
  (asdf:system-relative-pathname :test-grid-data #P"data/"))

(defvar *standard-db-file*
  (merge-pathnames #P"../../cl-test-grid-results/db.lisp" (src-dir)))

(defun add-run (run-info &optional (db *db*))
  (push run-info (getf db :runs)))

(defun print-list-elements (destination list separator elem-printer)
  (let ((maybe-separator ""))
    (dolist (elem list)
      (format destination maybe-separator)
      (funcall elem-printer elem)
      (setf maybe-separator separator))))

(defun print-list (destination list separator elem-printer)
  (format destination "(")
  (print-list-elements destination list separator elem-printer)
  (format destination ")"))

(defun print-test-status (destination status)
  (etypecase status
    (symbol (format destination "~s" status))
    (list (progn
            (let ((dest (or destination (make-string-output-stream))))
              (flet ((test-name-printer (test-name)
                       (format dest "~s" test-name)))
                (format dest "(:failed-tests ")
                (print-list dest (sort (copy-list (getf status :failed-tests))
                                       #'string<)
                            " " #'test-name-printer)
                (format dest " :known-to-fail ")
                (print-list dest (sort (copy-list (getf status :known-to-fail))
                                              #'string<)
                            " " #'test-name-printer)
                (format dest ")"))
              (if (null destination)
                  (get-output-stream-string dest)
                  nil))))))

(defun run-descr (run)
  "The description part of the test run."
  (getf run :descr))

(defun run-results (run)
  "The list of test suite statuses for every library in the specified test run."
  (getf run :results))

(defun (setf run-results) (new-run-results test-run)
  (setf (getf test-run :results) new-run-results))

(defun print-test-run (out test-run &optional (indent 0))
  (let ((descr (getf test-run :descr)))
    (format out
            "(:descr (:lisp ~s :lib-world ~s :time ~s :run-duration ~s :contact (:email ~s))~%"
            (getf descr :lisp)
            (getf descr :lib-world)
            (getf descr :time)
            (getf descr :run-duration)
            (getf (getf descr :contact) :email)))
  (format out "~v,0t:results (" (1+ indent))
  (print-list-elements out
                       (sort (copy-list (getf test-run :results))
                             #'string<
                             :key #'(lambda (lib-result)
                                      (getf lib-result :libname)))
                       (format nil "~~%~~~Dt" (+ indent 11))
                       #'(lambda (lib-result)
                           (format out
                                   "(:libname ~s :status ~a :test-duration ~s :log-byte-length ~s :log-blob-key ~s)"
                                   (getf lib-result :libname)
                                   (print-test-status nil (getf lib-result :status))
                                   (getf lib-result :test-duration)
                                   (getf lib-result :log-byte-length)
                                   (getf lib-result :log-blob-key))))
  (format out "))"))

(defun save-db (&optional (db *db*) (stream-or-path *standard-db-file*))
  (with-open-file (out stream-or-path
                       :direction :output
                       :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "(:version ~a~%" (getf db :version))
    (format out " :runs (")
    (print-list-elements out
                         (getf db :runs)
                         "~%~8t"
                         #'(lambda (test-run)
                             (print-test-run out test-run 8)))
    (format out "))")))

(defun read-db (&optional (stream-or-path *standard-db-file*))
  (with-open-file (in stream-or-path
                      :direction :input
                      :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                      )
    (test-grid-utils::safe-read in)))


#|
 DB version change history:
 0 - initial
 1 - cl-routes and cl-closure-template are renamed to routes and closure-template

|#