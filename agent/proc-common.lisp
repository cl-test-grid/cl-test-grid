;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; This file contains common utilities usefull for most
;;;; of the child lisp processes started by agent.

(in-package #:cl-user)

(defun set-response (response-file value)
  "Save the resposne for the parent process."
  (with-open-file (out response-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (pprint value out)))