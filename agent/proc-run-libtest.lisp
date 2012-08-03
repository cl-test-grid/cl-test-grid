;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; This file is loaded into a child lisp process to run a test suite using test-grid-testsuites::run-libtest.

(in-package :cl-user)

(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))

  ;; make test-grid.asd available for ASDF
  (pushnew (merge-pathnames "../" this-file-dir)
           asdf:*central-registry*
           :test #'equal)

  (load (merge-pathnames "proc-common.lisp" this-file-dir)))

(ql:quickload :test-grid-testsuites)

(defun setup-asdf-output-translations (private-quicklisp-dir asdf-output-root-dir)
  ;; Configure ASDF so that .fasl files from our private quicklisp
  ;; are stored in the specified output directory
  ;; (this allows us to ensure the libraries are freshly recompiled
  ;; at every test run, if every test run specifies different
  ;; temporary directory for .fasl files.
  ;;
  ;; .fasl files of everything except the libraries in the private quicklisp
  ;; go into the usual location (for example the .fasls of the cl-test-grid itself).

  ;; The only reliable way we found to customize where ASDF stores the .fasl
  ;; files is to redefine the asdf:apply-output-translations function.
  ;; Official ASDF approach of passing configuration DSL to
  ;; asdf:initialize-output-translations fails on some lisp/OSes,
  ;; whatever variant of the DSL config we use, because of
  ;; the implementation dependent behaviour of cl:pathname-match-p.

  ;; Note that unlike the default ASDF configuration,
  ;; we do not repeat the full path of .lisp file
  ;; inside the temporary directory for .fasl.
  ;; We just use <asdf-output-root-dir>/private-quicklisp/<library-archive-name>/*.fasl.
  ;; This saves us from problems of very long paths -
  ;; On Windows, without using a special notation,
  ;; maximum path length is 260 characters.

  (labels ((starts-with (sequence prefix &key (test #'eql))
             (let ((mismatch (mismatch sequence prefix :test test)))
               (or (null mismatch)
                   (>= mismatch (length prefix)))))

           ;; (assert (starts-with '("a" "b" "c") '("a" "B") :test #'string-equal))

           (child-path-p (parent-dir child)
             ;; Note, this implementation does not handle a/b/../b/c/ vs a/b/c/d and similar cases.
             ;; Also, we compare only directories, because sometime device is C: sometime is c:
             ;; we don't want to deal with such complexities, comparing only directories
             ;; is enough for us.
             (starts-with (pathname-directory child)
                          (pathname-directory parent-dir)
                          :test (if (member :asdf-windows *features*)
                                    #'string-equal
                                    #'string=)))
           (rel-path (dir child)
             (make-pathname :directory (append '(:relative)
                                               (nthcdr (length (pathname-directory dir))
                                                       (pathname-directory child)))
                            :name (pathname-name child)
                            :type (pathname-type child)
                            :version (pathname-version child))))
    (let ((orig-asdf-apply-output-translations #'asdf:apply-output-translations)
          (lib-dir (merge-pathnames (make-pathname :directory '(:relative "dists" "quicklisp" "software"))
                                    private-quicklisp-dir))
          (libs-output-dir (merge-pathnames (make-pathname :directory '(:relative "private-quicklisp"))
                                            asdf-output-root-dir))
          (test-grid-dir (asdf:system-source-directory :test-grid-testsuites))
          (test-grid-output-dir (merge-pathnames (make-pathname :directory '(:relative "test-grid"))
                                                 asdf-output-root-dir)))
      (asdf::defun* asdf:apply-output-translations (path)
        (cond ((child-path-p lib-dir path)
               (merge-pathnames (rel-path lib-dir path)
                                libs-output-dir))
              ((child-path-p test-grid-dir path)
               (merge-pathnames (rel-path test-grid-dir path)
                                test-grid-output-dir))
              (t
               (funcall orig-asdf-apply-output-translations path)))))))

(defun run-libtest-with-response-to-file (libname
                                          run-descr
                                          logfile
                                          private-quicklisp-dir
                                          asdf-output-root-dir
                                          response-file)

  (setup-asdf-output-translations private-quicklisp-dir asdf-output-root-dir)

  (let ((lib-result (test-grid-testsuites::run-libtest libname run-descr logfile)))
    (set-response response-file lib-result)))
