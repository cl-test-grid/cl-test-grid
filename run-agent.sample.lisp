;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; Example file for how to configure and run cl-test-grid agent.
;;;; Load the file by:
;;;;        (load "run-agent.lisp")
;;;;

(defparameter *this-dir*
  (make-pathname :name nil :type nil :defaults *load-truename*))

;; If quicklisp is absent, install and load it
(load (merge-pathnames "agent/require-quicklisp.lisp" *this-dir*))
(tg-require-quicklisp:require :agent-work-dir (merge-pathnames "work-dir/agent/" *this-dir*))

(let ((asdf:*central-registry* (cons *this-dir* asdf:*central-registry*)))
  (ql:quickload :test-grid-agent))

;;; Lisp implementations we have on this machine.
(defparameter *abcl* (make-instance 'lisp-exe:abcl
                                    :java-exe-path "java"
                                    :abcl-jar-path "/home/testgrid/lisps/abcl-1.0.1\\abcl.jar"))
(defparameter *clisp* (make-instance 'lisp-exe:clisp :exe-path "/home/testgrid/lisps/clisp-2.49/clisp"))
(defparameter *ccl-1.8-x86* (make-instance 'lisp-exe:ccl
                                           :exe-path "/home/testgrid/lisps/ccl-1.8/wx86cl"))
(defparameter *ccl-1.8-x86-64* (make-instance 'lisp-exe:ccl
                                              :exe-path "/home/testgrid/lisps/ccl-1.8/wx86cl64"))
(defparameter *sbcl* (make-instance 'lisp-exe:sbcl :exe-path "/home/testgrid/lisps/sbcl-1.0.57/bin/sbcl"))
(defparameter *sbcl-git* (make-instance 'lisp-exe:sbcl :exe-path "/home/testgrid/lisps/sbcl-git-bin/run.sh"))
(defparameter *cmucl* (make-instance 'lisp-exe:cmucl :exe-path "/home/testgrid/lisps/cmucl-20c/bin/lisp"))
;; ECL provides two compilers: bytecode compiler and lisp to C compiler.
;; What compiler to test is specified explicitly as a parameter
;; to lisp-exe:ecl constructor.
(defparameter *ecl-bytecode* (make-instance 'lisp-exe:ecl
                                            :exe-path "/home/testgrid/lisps/ecl/bin/ecl"
                                            :compiler :bytecode))
;; Note, if you are specifying ECL to use lisp-to-c
;; compiler, you should have a C compiler available
;; in PATH.
(defparameter *ecl-lisp-to-c* (make-instance 'lisp-exe:ecl
                                             :exe-path "/home/testgrid/lisps/ecl/bin/ecl"
                                             :compiler :lisp-to-c))
(defparameter *acl* (make-instance 'lisp-exe:acl :exe-path "/home/testgrid/lisps/acl82express/alisp"))

;; create agent instance
(defparameter *agent*
  (test-grid-agent:make-agent
   :lisps (list *abcl* *clisp* *ccl-1.8-x86*
                *ccl-1.8-x86-64* *sbcl*
                *ecl-bytecode*
                *ecl-lisp-to-c*
                *acl*
                ;; pre-release version of SBCL goes to test-grid-storage
                ;; named "sbcl" instead of the default storage "main"
                (list *sbcl-git* "sbcl"))

   ;; Preferred lisp is the lisp implementation the agent
   ;; uses when it needs to perform some auxiliary
   ;; task in a separate lisp process (for example, updating
   ;; the private quicklisp installation we run tests on).
   ;; Untill quicklisp issue https://github.com/quicklisp/quicklisp-client/issues/71
   ;; is resolved, we suggest to not use CCL here.
   :preferred-lisp *sbcl*

   ;; Please provide your email so that we know who is submitting the test results.
   ;; Also the email will be published in the online reports, and the library
   ;; authors can later contact you in case of questions about this test run,
   ;; your environment, etc.
   ;;
   ;; If you are strongly opposed to publishing you email, please provide
   ;; just some nickname.
   :user-email "avodonosov@yandex.ru"

   ;;; --- the settings below are not required ---

   ;; You may specify custom working directory for
   ;; the agent. By default it is <source-code-base>/work-dir/agent/
   ;;
   ;; :work-dir #P"/my/cl-test-grid/work-dir/agent/"

   ;; Only if you run several agents, give each of them
   ;; different work-dir (above) and lock port:
   ;;
   ;; :singleton-lock-port 7686
   ))

;;; Ask agent to do its work
(test-grid-agent:main *agent*)
