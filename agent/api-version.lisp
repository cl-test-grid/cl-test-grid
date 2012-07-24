;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

(defparameter +api-version+ '(1 . 0)
  "Current version of the test-grid-agent API.")

(defun major (version) (car version))
(defun minor (version) (cdr version))

(defun api-compatible-p (version-required
                         &optional (version-provided +api-version+))
  "Returns true the API version VERSION-PROVIDED
is compatible with the VERSION-REQIRED. The version are conses
in the form (<major> . <minor>), where major and minor are
non-negative integers, for example '(1 . 0)."
  (and (= (major version-provided) (major version-required))
       (>= (minor version-provided) (minor version-required))))

(assert (api-compatible-p '(2 . 1) '(2 . 2)))
(assert (api-compatible-p '(2 . 2) '(2 . 2)))
(assert (not (api-compatible-p '(2 . 3) '(2 . 2))))
(assert (not (api-compatible-p '(3 . 1) '(2 . 2))))
(assert (not (api-compatible-p '(1 . 1) '(2 . 2))))
