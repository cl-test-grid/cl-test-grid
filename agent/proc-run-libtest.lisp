;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; This file is loaded into a child lisp process to run a test suite using test-grid-testsuites::run-libtest.

(in-package :cl-user)

;;; Configure away the normal ASDF source registry, so we only use Quicklisp.
(asdf:initialize-source-registry `(:source-registry :ignore-inherited-configuration))

(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))

  ;; make test-grid.asd available for ASDF
  (pushnew (merge-pathnames "../" this-file-dir)
           asdf:*central-registry*
           :test #'equal))

(ql:quickload :test-grid-testsuites)

(defun setup-asdf-output-translations (private-quicklisp-dir asdf-output-root-dir)
  (let (;; Configure ASDF so that .fasl files from our private quicklisp
        ;; are stored in the specified output directory
        ;; (this allows us to ensure the libraries are freshly recompiled
        ;; at every test run, if every test run specifies different
        ;; temporary directory for .fasl files.
        (lib-dir private-quicklisp-dir)
        (libs-output-dir (merge-pathnames (make-pathname :directory '(:relative "private-quicklisp"))
                                          asdf-output-root-dir))

        ;; The .fasl files of test-grid-testsuites also stored in custom
        ;; output directory.
        (test-grid-dir (asdf:system-source-directory :test-grid-testsuites))
        (test-grid-output-dir (merge-pathnames (make-pathname :directory '(:relative "test-grid"))
                                               asdf-output-root-dir)))

    (add-asdf-output-translation lib-dir libs-output-dir)
    (add-asdf-output-translation test-grid-dir test-grid-output-dir)))

(defun run-libtest (libname)
  (catching-problems (lambda ()
                       (test-grid-testsuites::normalize-status (test-grid-testsuites:libtest libname)))
                     (lambda ()
                       (return-from run-libtest :fail))))

(defun run-libtest-main (libname
                         log-file
                         private-quicklisp-dir
                         asdf-output-root-dir)
  (setup-asdf-output-translations private-quicklisp-dir asdf-output-root-dir)
  (saving-output log-file
                 (lambda ()
                   (format t "  *features*:        ~(~A~)~%~%" (sort (copy-list *features*) #'string<))
                   (run-libtest libname))))

