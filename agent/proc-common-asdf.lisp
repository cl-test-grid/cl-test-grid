;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; This file contains common utilities usefull for most
;;;; of the child lisp processes started by agent.

(in-package #:cl-user)

(defvar *cl-test-grid-output-translations* '())

(defun add-asdf-output-translation (source-dir output-dir)
  (pushnew (list source-dir output-dir)
           *cl-test-grid-output-translations*
           :test #'equal))

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

(let ((orig-asdf-apply-output-translations #'asdf:apply-output-translations))
  (defun apply-cl-test-grid-output-translations (path)
    (labels ((source-dir (translation) (first translation))
             (output-dir (translation) (second translation))
             (starts-with (sequence prefix &key (test #'eql))
               (let ((mismatch (mismatch sequence prefix :test test)))
                 (or (null mismatch)
                     (>= mismatch (length prefix)))))

             ;; (assert (starts-with '("a" "b" "c") '("a" "B") :test #'string-equal))

             (child-path-p (child parent-dir)
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
      ;; ASDF output translations design requires that sources
      ;; contained in the output directory of some translation
      ;; should have their .fasl files in the same directory,
      ;; no other translations should be applied to them.
      ;; See this asdf-devel thread:
      ;; http://lists.common-lisp.net/pipermail/asdf-devel/2013-April/003129.html
      (when (find path
                  *cl-test-grid-output-translations*
                  :key #'output-dir
                  :test #'child-path-p)
        (return-from apply-cl-test-grid-output-translations path))
      (let ((translation (find path
                               *cl-test-grid-output-translations*
                               :key #'source-dir
                               :test #'child-path-p)))
        (if translation
            (merge-pathnames (rel-path (source-dir translation) path)
                             (output-dir translation))
            (funcall orig-asdf-apply-output-translations path))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol (string '#:defun*) '#:asdf)
    (pushnew :cl-test-grid-asdf-has-defun* *features*)))

;; For old ASDF versions.
#+cl-test-grid-asdf-has-defun*
(asdf::defun* asdf:apply-output-translations (path)
  (apply-cl-test-grid-output-translations path))

;; New ASDF:
#-cl-test-grid-asdf-has-defun*
(setq uiop:*output-translation-function* 'apply-cl-test-grid-output-translations)


#|  Test:

(let ((*cl-test-grid-output-translations* '()))
  (add-asdf-output-translation #P "/" #P "/default/target/dir/")
  (add-asdf-output-translation #P "/source/dir/" #P "/target/dir/")

  (assert (equal #P"/target/dir/file.lisp"
                 (apply-cl-test-grid-output-translations #P"/source/dir/file.lisp"))
          ()
          "Output translation for a specific source dir must overrride the default")

  (assert (equal #P"/default/target/dir/other/dir/file.lisp"
                 (apply-cl-test-grid-output-translations #P"/other/dir/file.lisp"))
          ()
          "The default translation must workd")

  (assert (equal #P"/target/dir/file.lisp"
                 (apply-cl-test-grid-output-translations #P"/target/dir/file.lisp"))
          ()
          "A file located in the target dir must remain untranslated, despite the target dir is under our default source dir:"))



|#
