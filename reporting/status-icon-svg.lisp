;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(defpackage #:test-grid-reporting/status-icon-svg
  (:use #:common-lisp)
  (:nicknames #:tg-reporting/status-icon-svg #:tg-rep/status-icon-svg))

(in-package :tg-reporting/status-icon-svg)


(defun render (elem)
  "A functional template engine to generate strings.
ELEM is either an atom, which is simply converted to string,
or a list. List is rendered by concatenation
of its elements, recursively."
  (etypecase elem
    (string elem)
    (number (prin1-to-string elem))
    ;; TODO: reduce garbadge by not creating
    ;; intermediate RENDER result strings.
    (list (apply #'concatenate
                 'string
                 (mapcar #'render elem)))))

(assert (string= "abc" (render "abc")))
(assert (string= "" (render nil)))
(assert (string= "abcdef" (render '("abc" "def"))))
(assert (string= "abcdefxyz" (render '("abc" ("def") "xyz"))))

(defun result-rect (status-code lisp-num ql-num)
  (let ((color (ecase status-code
                 (:ok "rgb(0,255,0)")
                 (:fail "rgb(255,0,0)")
                 (:none "rgb(127,127,127)")))
        (y lisp-num)
        (x (* 25 ql-num)))
    `("<rect x=\"" ,x "\" y=\"" ,y "\" width=\"25\" height=\"1\" style=\"fill:" ,color "\" />")))

(defun result-rect2 (result lisp-num ql-num)
  (let ((class (ecase result
                 (:ok "ok")
                 (:fail "fail")
                 (:none "none")))
        (y lisp-num)
        (x (* 25 ql-num)))
    `("<rect x=\"" ,x "\" y=\"" ,y "\" class=\"" ,class "\"/>")))

(defun status-svg (input &key width height title-p)
  (let ((title-width 55))
    (render `("<svg width=\"" ,width "\" height=\"" ,height "\""
                   "viewBox=\"0 0 "
                             ,(if title-p (+ 50 title-width) 50) " "
                             ,(length input) "\""
                   "preserveAspectRatio=\"none\""
                   "style=\"background-color: #555\""
                   ">"
                 "<style>"
                   ".ok {"
                      "fill: rgb(0,255,0);"
                      "width: 25px;"
                      "height: 1px"
                   "}"
                   ".fail {"
                      "fill: rgb(255,0,0);"
                      "width: 25px;"
                      "height: 1px"
                   "}"
                   ".none {"
                      "fill:rgb(127,127,127);"
                      "width:25px;"
                      "height:1px"
                   "}"
              "</style>"
              ,(when title-p
                 `("<g transform=\"translate(" ,title-width ")\">"))
              ,(let ((n -1))
                 (mapcar (lambda (r)
                           (incf n)
                           (list (result-rect2 (first r) n 0)
                                 (result-rect2 (second r) n 1)))
                         input))
              ,(when title-p
                 `("</g>"
                   "<text fill=\"#fff\" x=\"3\" y=\"" "0.6em" "\" font-size=\"" "0.6em" "\">cl-test-grid </text>"))
             "</svg>")
  )))
