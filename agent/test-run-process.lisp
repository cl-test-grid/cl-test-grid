;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;b
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(in-package :cl-user)

(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))
  (pushnew (merge-pathnames "../" this-file-dir)
           asdf:*central-registry*
           :test #'equal))

(asdf:operate 'asdf:load-op :test-grid)

(defun run-with-response-to-file (test-suites output-base-dir user-email response-file)
  (let ((results-dir (test-grid::run-libtests :libs test-suites
                                              :output-base-dir output-base-dir
                                              :user-email user-email)))
    (test-grid::write-to-file results-dir response-file)))

