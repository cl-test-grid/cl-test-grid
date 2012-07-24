;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

(defvar *id-random-sate* nil)

(defun generate-id ()
  "Generates a random ID - a 10 digits sting (digits in radix 36)."
  ;; Implementation note: often random state is initialized
  ;; from the current time stamp as returned
  ;; by get-universal-time, i.e. with resolution
  ;; to seconds. An as RANDOM returns deterministic
  ;; values based on the random state, the IDs returned
  ;; by the current implementation are no more
  ;; unique than timestamps. But it's enough for
  ;; our current needs.
  (when (null *id-random-sate*)
    (setf *id-random-sate* (make-random-state t)))
  (string-upcase
   (format nil "~36,10,'0r" (random #.(expt 36 10)
                                    *id-random-sate*))))