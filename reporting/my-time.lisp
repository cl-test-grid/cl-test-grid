;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defmacro my-time ((title) &body body)
  "Ensure meaningful title is printed before the
timing (CCL's TIME prings the code, but SBCL's prints nothing before
the timing, so it's difficult to see what is what when many TIME
calls are used in the code."
  `(progn
     (format t ,title)
     (fresh-line)  
     (time ,@body)))
