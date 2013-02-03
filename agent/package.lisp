;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(defpackage #:test-grid-agent
  (:nicknames :tg-agent)
  (:use #:common-lisp)
  (:export
           ;; agent public API version
           #:+api-version+
           #:api-compatible-p

           ;; the agent class (abstract)
           #:agent

           ;; agent configuration properties,
           ;; without default values, must be configured by user:
           #:lisps
           #:preferred-lisp
           #:user-email
           ;; have default values:
           #:work-dir
           #:singleton-lock-port

           ;; agent factory function
           #:make-agent

           ;; agent main function
           #:main))

(in-package #:test-grid-agent)

(defparameter +api-version+ '(1 . 2)
  "Current version of the test-grid-agent API.")

(defgeneric api-compatible-p (version-required &optional version-provided)
  (:documentation "Returns true if an interface VERSION-PROVIDED by some module
is compatible with the VERSION-REQIRED by the module client.

The version are conses in the form (<major> . <minor>),
where major and minor are non-negative integers, for example '(1 . 0).

VERSION-PROVIDED defaults to TEST-GRID-AGENT:+API-VERSION+"))

(defclass agent ()
   ;; The list of lisp-exe's to run tests on.
  ((lisps :type list :accessor lisps :initform nil)
   ;; The lisp-exe considered as more reliable on this OS,
   ;; and supporting more libraries. Used run various small
   ;; lisp programs like quicklisp update.
   (preferred-lisp :type (or null lisp-exe:lisp-exe) :accessor preferred-lisp :initform nil)
   (user-email :type (or null string) :accessor user-email :initform nil)
   ;; pathname-designator for the working directory,
   ;; defaults to <source code root>/work-dir/agent
   (work-dir :accessor work-dir)
   ;; the tcp port used as a lock to prevent several agents
   ;; running simultaneously.
   ;; If you want to ran several agents, assign them all
   ;; different signlethon-ports and different work-dirs.
   (singleton-lock-port :type fixnum :accessor singleton-lock-port :initform 7685)))


(defgeneric make-agent ())

(defgeneric main (agent))