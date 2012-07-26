;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; Example file for how to configure and run cl-test-grid agent.
;;;; This file is supposed to be LOADed by:
;;;;        (load "run-agent.lisp")
;;;;

;; Put the directory containing this file
;; into ASDF:*CENTRAL-REGISTRY*
;; so that ASDF can find the test-grid-agent.asd
(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))
  (pushnew this-file-dir asdf:*central-registry* :test #'equal))

(ql:quickload :test-grid-agent)

;; ensure this script is not outdated in respect to
;; the agent public API
(let ((api-version-required '(1 . 0)))
  (when (not (test-grid-agent:api-compatible-p api-version-required))
    (error "The agent public API has changed in an incompatible way:
current agent API version is ~A. We use version ~A.
It's necessary to adjust the run-agent.lisp script (see
run-agent.sample.lisp for a fresh example)."
           test-grid-agent:+api-version+ api-version-required)))

;; create agent instance
(defparameter *agent* (test-grid-agent:make-agent))

;;; Now inform the *AGENT* about the lisp implementations
;;; we have on this machine.
(defparameter *abcl* (make-instance 'lisp-exe:abcl
                                    :java-exe-path "java"
                                    :abcl-jar-path "C:\\Users\\anton\\unpacked\\abcl\\abcl-bin-1.0.1\\abcl.jar"))
(defparameter *clisp* (make-instance 'lisp-exe:clisp :exe-path "clisp"))
(defparameter *ccl-1.7-x86* (make-instance 'lisp-exe:ccl
                                           :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.7-windows\\wx86cl.exe"))
(defparameter *ccl-1.7-x86-64* (make-instance 'lisp-exe:ccl
                                              :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.7-windows\\wx86cl64.exe"))
(defparameter *ccl-1.8-x86* (make-instance 'lisp-exe:ccl
                                           :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl.exe"))
(defparameter *ccl-1.8-x86-64* (make-instance 'lisp-exe:ccl
                                              :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl64.exe"))
(defparameter *sbcl* (make-instance 'lisp-exe:sbcl :exe-path "sbcl"))
(defparameter *cmucl* (make-instance 'lisp-exe:cmucl :exe-path "/opt/cmucl-20c/bin/lisp"))
;; ECL provides two compilers: bytecode compiler and lisp to C compiler.
;; What compiler to test is specified explicitly as a parameter
;; to lisp-exe:ecl constructor.
(defparameter *ecl-bytecode* (make-instance 'lisp-exe:ecl
                                            :exe-path "C:\\Users\\anton\\projects\\ecl\\bin\\ecl.exe"
                                            :compiler :bytecode))
;; Note, if you are specifying ECL to use lisp-to-c
;; compiler, you should have a C compiler available
;; in PATH.
(defparameter *ecl-lisp-to-c* (make-instance 'lisp-exe:ecl
                                             :exe-path "C:\\Users\\anton\\projects\\ecl\\bin\\ecl.exe"
                                             :compiler :lisp-to-c))
(defparameter *acl* (make-instance 'lisp-exe:acl :exe-path "C:\\Program Files (x86)\\acl82express\\alisp.exe"))

(setf (test-grid-agent:lisps *agent*) (list *abcl* *clisp* *ccl-1.8-x86*
                                            *ccl-1.8-x86-64* *ccl-1.7-x86*
                                            *ccl-1.7-x86-64* *sbcl*
                                            *ecl-bytecode*
                                            *ecl-lisp-to-c*
                                            *acl*)

      ;; Preferred lisp is the lisp implementation the agent
      ;; uses when it needs to perform some auxiliary
      ;; task in a separate lisp process - for example, updating quicklisp.
      ;; Almost always this is the same lisp that runs the agent
      ;; process itselft (i.e. the lisp which runs this script).
      (test-grid-agent:preferred-lisp *agent*) *ccl-1.8-x86*

      ;; Please provide your email so that we know who is submitting the test results.
      ;; Also the email will be published in the online reports, and the library
      ;; authors can later contact you in case of questions about this test run,
      ;; your environment, etc.
      ;;
      ;; If you are strongly opposed to publishing you email, please provide
      ;; just some nickname.
      (test-grid-agent:user-email *agent*) "avodonosov@yandex.ru")

;;; Ask agent to do it's work
(test-grid-agent:main *agent*)
