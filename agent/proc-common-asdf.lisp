;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; This file contains common utilities usefull for most
;;;; of the child lisp processes started by agent.

(in-package #:cl-user)

(defun add-asdf-output-translation (source-dir output-dir)
  ;; The only reliable way we found to customize where ASDF stores the .fasl
  ;; files is to redefine the asdf:apply-output-translations function.
  ;; Official ASDF approach of passing configuration DSL to
  ;; asdf:initialize-output-translations fails on some lisp/OSes,
  ;; whatever variant of the DSL config we use, because of
  ;; the implementation dependent behaviour of cl:pathname-match-p.

  ;; Note that unlike the default ASDF configuration,
  ;; we do not repeat the full path of .lisp file
  ;; inside the temporary directory for .fasl.
  ;; We just use <asdf-output-root-dir>/private-quicklisp/<library-relative-path>/*.fasl.
  ;; This saves us from problems of very long paths -
  ;; On Windows, without using a special notation,
  ;; the maximum path length is 260 characters.

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
    (let ((orig-asdf-apply-output-translations #'asdf:apply-output-translations))
      (asdf::defun* asdf:apply-output-translations (path)
        (if (child-path-p source-dir path)
            (merge-pathnames (rel-path source-dir path) output-dir)
            (funcall orig-asdf-apply-output-translations path))))))
