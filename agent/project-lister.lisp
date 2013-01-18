;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

;;; Project lister is an object able to list
;;; the projects we are going to test, and their
;;; ASDF systems. It has the following interface.
(defgeneric project-names (project-lister))
(defgeneric project-systems (project-lister project-name))

;;; Implementation

(defclass project-lister ()
  ;; keeps information in an alist
  ;; (("project-name" . ("system1" "system2" ... "systemN"))
  ;;  ("another-project" . ("sys1" "sys2" ... "sysM"))
  ;;  ...)
  ((project-systems-alist :type list
                          :initarg :project-systems-alist
                          :accessor project-systems-alist)))

(defmethod project-names ((project-lister project-lister))
  (mapcar #'first (project-systems-alist project-lister)))

(defmethod project-systems ((project-lister project-lister) project-name)
  (rest (assoc (string-downcase project-name)
               (project-systems-alist project-lister)
               :test #'string=)))

(defparameter +list-quicklisp-projects-timeout-seconds+ 60)

(defun proc-list-quicklisp-projects (lisp-exe private-quicklisp-dir)
  (with-response-file (response-file)
    (let* ((code `(progn
                    (load ,(merge-pathnames "setup.lisp" private-quicklisp-dir))
                    (load ,(src-file "proc-common.lisp"))
                    (load ,(src-file "proc-list-quicklisp-projects.lisp"))
                    (cl-user::set-response ,response-file
                                           (cl-user::list-quicklisp-projects)))))
      (log:info "Retrieving the list of projects and their ASDF systems from the Quicklisp version we are going to test...")
      (lisp-exe:run-with-timeout +list-quicklisp-projects-timeout-seconds+ lisp-exe code))))


(defun init-project-lister (lisp-exe private-quicklisp-dir)
  (let ((alist (proc-list-quicklisp-projects lisp-exe private-quicklisp-dir)))
    (make-instance 'project-lister :project-systems-alist alist)))