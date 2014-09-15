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

(defun setup-asdf-output-translations (private-quicklisp-dir asdf-output-root-dir)
  (let (;; Configure ASDF so that .fasl files from our private quicklisp
        ;; are stored in the specified output directory
        ;; (this allows us to ensure the libraries are freshly recompiled
        ;; at every test run, if every test run specifies different
        ;; temporary directory for .fasl files.
        (quicklisp-output-dir (merge-pathnames (make-pathname :directory '(:relative "ql"))
                                               asdf-output-root-dir))

        ;; The .fasl files of test-grid-testsuites also stored in custom
        ;; output directory.
        (test-grid-dir (asdf:system-source-directory :test-grid-testsuites))
        (test-grid-output-dir (merge-pathnames (make-pathname :directory '(:relative "test-grid"))
                                               asdf-output-root-dir)))

    (add-asdf-output-translation private-quicklisp-dir quicklisp-output-dir)
    (add-asdf-output-translation test-grid-dir test-grid-output-dir)))

(defmacro fncall (funname &rest args)
  `(funcall (read-from-string ,funname) ,@args))

(defun run-libtest (libname)
  (wrap-status
    (ql:quickload :test-grid-testsuites)
    (fncall "test-grid-testsuites::normalize-status" (fncall "test-grid-testsuites:libtest" libname))))

(defun run-libtest-main (libname
                         log-file
                         private-quicklisp-dir
                         asdf-output-root-dir)
  (setup-asdf-output-translations private-quicklisp-dir asdf-output-root-dir)
  (capturing-io log-file
                (lambda ()
                  (let ((*package* (find-package :keyword)))
                    (format t "  *features*:        ~(~S~)~%" (sort (copy-list *features*) #'string<)))
                  (format t "  ASDF version:      ~A~%" (asdf:asdf-version))
                  (format t "============================================================~%~%")
                  (run-libtest libname))))

