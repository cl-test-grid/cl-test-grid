;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; This file is loaded into a child lisp process to retrieve
;;;; the list of projects and their ASDF systems provided
;;;; by the quicklisp distro version available to that
;;;; child lisp process.

(in-package :cl-user)

(defun list-quicklisp-projects()
  "Returns alist specifying mapping
from project name to a list of the ASDF
systems provided by that project."
  (mapcar (lambda (release)
            (cons (ql-dist:name release)
                  (mapcar #'ql-dist:name (ql-dist:provided-systems release)))) 
          (let ((releases (ql-dist:provided-releases (ql-dist:dist "quicklisp"))))
            ;; Workdaround for the quiclisp issue 61
            ;; https://github.com/quicklisp/quicklisp-client/issues/61
            ;; Don't include release, if quicklisp can't find it by it's name
            (remove-if-not #'ql-dist:release releases :key #'ql-dist:name))))
