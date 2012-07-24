;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

(defclass agent ()
   ;; The list of lisp-exe's to run tests on.
  ((lisps :type list :accessor lisps :initform nil)
   ;; The lisp-exe considered as more reliable on this OS,
   ;; and supporting more libraries. Used run various small
   ;; lisp programs like quicklisp update.
   (preferred-lisp :type (or null lisp-exe) :accessor preferred-lisp :initform nil)
   (user-email :type (or null string) :accessor user-email :initform nil)
   ;; ------ package private ------
   (persistence :type persistence :accessor persistence)
   (blobstore :accessor blobstore)))

;;; File system roots:
(defun work-dir ()
  (merge-pathnames "work-dir/agent/"
                   test-grid-config::*src-base-dir*))

(defun src-dir ()
  "File system location of test-grid-agent source code"
  (merge-pathnames "agent/"
                   test-grid-config::*src-base-dir*))

;;; Working directory structure
(defun test-output-base-dir ()
  (merge-pathnames "test-runs/"
                   (work-dir)))

(defun log-file ()
  ;; good thing about log4cl, it creates
  ;; intermediate directories automatically,
  ;; so we need not care about this
  (merge-pathnames "logs/agent.log"
                   (work-dir)))

;; File relative to the work-dir
(defun workdir-file (relative-path)
  (merge-pathnames relative-path (work-dir)))

(defun persistence-file ()
  (workdir-file "persistence.lisp"))

;; File relative to the src-dir
(defun src-file (file-name)
  (merge-pathnames file-name (src-dir)))

;; TODO:
;; * Level 1 beta
;; + test runs in work-dir/agent/test-runs
;; + configuration: configure list of lisps, user email.
;; + configuration: test-config procedcure (try to run all the lisps configured)
;; + configuration: contributor email as a configurabel agent propery,
;;   instead of cl-test-grid-settings.lisp
;; + delete test run data after the results have been submitted
;; * Level 1 stable
;; + full path to files: setup, proc-run-libtests, response-file
;; + timeout
;; + check status of the quicklisp update process
;; - Prevent child lisp process entering debugger.
;; - Different lisps treat unhangled signals during -eval
;;   differently: ECL exits with status 1, CCL enters debugger
;;   and hangs.
;; - program parameters escaping is not perfect. When we
;;   run CLISP as an external process, it can not stand
;;   string literals with " inside.
;; - enable/disable program parameters escapting depending on the
;;   external-program behaviour (consider also using input stream
;;   of the lisp process, or a temporary file)
;; - prevent test run directory names conflict (currently they
;;   are named by timestamp with resolution to seconds)
;; - temp file naming: ensure unique [probably specify random-state]
;; * Level 2
;; - do we need node-id (or agent-id)? should it be
;;   meaningfull string provided by user, or an
;;   automatically generated one? Note, this value is different
;;   from the contributor email, because one contributor
;;   may have more than one machine.
;; * Backward compatibility
;; + remove the old test-runs directory
;; - remove the old ~/cl-test-grid-settings.lisp
;;   (or rename it to ~/cl-test-grid-settings.lisp.unused?)
;; * Pretty/convenience
;; - persistence.lisp format - sort and newline for every record
;; - cl-test-grid-config.lisp should work via public API,
;;   instead of working directly inside of the test-grid package.

(defun update-testing-quicklisp (lisp-exe)
  (log:info "Ensuring the quicklisp used to download the libraries being tested is updated to the recent version...")
  (let ((quicklisp-version
         (with-response-file (response-file)
           (lisp-exe:run-lisp-process lisp-exe
                                      `(load ,(truename (src-file "proc-common.lisp")))
                                      `(load ,(truename (src-file "proc-update-quicklisp.lisp")))
                                      `(cl-user::set-response ,response-file (cl-user::do-quicklisp-update))))))
    (log:info "Quicklisp update process finished, current quicklisp version: ~A." quicklisp-version)
    quicklisp-version))

;;; Configuration check functions
(defun lisp-process-echo (lisp-exe str-to-echo)
  (with-response-file (response-file)
    (lisp-exe:run-lisp-process lisp-exe
                               `(load ,(truename (src-file "proc-common.lisp")))
                               `(cl-user::set-response ,response-file ,str-to-echo))))

(defgeneric check-lisp (lisp-exe)
  (:method ((lisp-exe lisp-exe:lisp-exe))
    (let* ((echo-string "abcs *() { /d e11 ")
         (response (lisp-process-echo lisp-exe echo-string)))
    (when (not (string= echo-string response))
      (error "Lisp ~A returned string ~S while we exected ~S."
             lisp-exe response echo-string)))))

(defmethod check-lisp ((lisp-exe lisp-exe:ecl))
  (call-next-method)
  (when (eq :lisp-to-c (lisp-exe:compiler lisp-exe))
    (log:info "Checking ECL lisp-to-c compiler - it may be not working if a C compiler is absent in PATH.")
    (let ((compile-failure-p
           ;; We will try to compile a function and see if it is successful.
           ;; We can not rely on the 3rd value returned by cl:compile
           ;; because in the old ECL versions there was a bug
           ;; that cl:compile didn't reported problems when running
           ;; C compiler:
           ;; https://sourceforge.net/tracker/?func=detail&atid=398053&aid=3546963&group_id=30035
           ;; After some time, when these old ECL versions will not
           ;; be relevant to support, we could probably change
           ;; the implementation to check the 3rd value
           ;; returned by cl:compile. Untill then, we rely
           ;; on the printed representation of the compile
           ;; function.
           (with-response-file (response-file)
             (lisp-exe:run-lisp-process
              lisp-exe
              `(load ,(truename (src-file "proc-common.lisp")))
              '(compile (defun cl-user::-unique-name--- ()))
              `(cl-user::set-response ,response-file
                                      (not (string= "#<compiled-function -UNIQUE-NAME--->"
                                                    (format nil "~A" (function cl-user::-unique-name---)))))))))
      (when compile-failure-p
        (error "ECL implementation ~A can not compile functions. Ensure that C compiler is available in PATH."
               lisp-exe)))))

(defun check-config (agent)
  (log:info "Checking configuration...")
  (when (zerop (length (user-email agent)))
    (error "The user-email property of test-grid agent can not be empty. Please specify your email, or at least some nickname if you are strongly opposed to publish your email."))
  (log:info "Checking external process functionality for the preffered lisp ~A..."
            (preferred-lisp agent))
  (check-lisp (preferred-lisp agent))
  (dolist (lisp (lisps agent))
    (log:info "Checking external process functionality for ~A..." lisp)
    (check-lisp lisp)
    (log:info "~A OK" lisp))
  (log:info "All the external lisps passed the configuration check OK")
  t)

(defgeneric implementation-identifier (lisp-exe)
  (:method ((lisp-exe lisp-exe:lisp-exe))
    (let ((ql-setup-file (workdir-file "quicklisp/setup.lisp")))
      (when (not (probe-file ql-setup-file))
        (error "Can not determine lisp implemntation name until quicklisp is installed - we need ASDF installed together with quicklisp to evaluate (asdf::implementation-identifier)."))
      (with-response-file (response-file)
        (lisp-exe:run-lisp-process lisp-exe
                                   `(load ,(truename (src-file "proc-common.lisp")))
                                   ;; can only do after quicklisp is installed
                                   `(load ,(truename ql-setup-file))
                                   `(cl-user::set-response ,response-file
                                                           ;; read from string is necessary
                                                           ;; because if agent is run on say CCL,
                                                           ;; then ASDF:IMPLEMENTATION-IDENTIFIER
                                                           ;; is exported from ASDF. But if the
                                                           ;; child process is CLISP, on it
                                                           ;; the symbol is not exported, and
                                                           ;; CLISP cant read the code s-expr we pass to it.
                                                           (funcall (read-from-string
                                                                     "asdf::implementation-identifier"))))))))
(defmethod implementation-identifier ((lisp-exe lisp-exe:ecl))
  ;; append type of compiler (bytecode or lisp-to-c)
  ;; to the implementation identifier
  (format nil "~A-~A"
          (call-next-method)
          (string-downcase (lisp-exe:compiler lisp-exe))))

;; note: not thread-safe
(fare-memoization:memoize 'implementation-identifier)

(defun divide (list predicate)
  "Returns two lists, the first with elements satisfying
the PREDICATE, and the second with elements not satisfying
the PREDICATE."
  (let (positive negative)
    (dolist (elem list)
      (if (funcall predicate elem)
          (push elem positive)
          (push elem negative)))
    (values positive negative)))

(multiple-value-bind (p n) (divide '(1 2 3) #'oddp)
  (assert (alexandria:set-equal p '(1 3)))
  (assert (alexandria:set-equal n '(2))))

(defmacro divide-into (list predicate
                       (satisfying-var not-satisfying-var)
                       &body body)
  `(multiple-value-bind (,satisfying-var ,not-satisfying-var)
       (divide ,list ,predicate)
     ,@body))

(divide-into '(1 2 3) #'oddp
    (p n)
  (assert (alexandria:set-equal p '(1 3)))
  (assert (alexandria:set-equal n '(2))))

;;; Main program

(defun run-tests (agent lib-world)

  (divide-into (lisps agent) (lambda (lisp)
                               (tested-p (persistence agent)
                                         lib-world
                                         (implementation-identifier lisp)))
      (done-lisps pending-lisps)
    (when done-lisps
      (log:info "Skipping the lisps already tested on ~A: ~S"
                lib-world
                (mapcar #'implementation-identifier done-lisps)))
    (dolist (lisp pending-lisps)
      (handler-case
          (progn
            (log:info "Running tests for ~A" (implementation-identifier lisp))
            (let ((results-dir (perform-test-run lib-world
                                                 lisp
                                                 test-grid::*all-libs*
                                                 (test-output-base-dir)
                                                 (user-email agent))))
              (submit-test-run-results (blobstore agent) results-dir)
              (mark-tested (persistence agent) lib-world (implementation-identifier lisp))
              (cl-fad:delete-directory-and-files results-dir :if-does-not-exist :ignore)))
        (serious-condition (e)
          (log:error "Error during tests on ~A: ~A. Continuing for remaining lisps."
                     (implementation-identifier lisp) e))))))

(defun ensure-has-id (agent)
  (let* ((p (persistence agent))
         (id (get-agent-id p)))
    (if id
        (log:info "agent-id: ~A" id)
        (progn
          (set-agent-id p (generate-id))
          (log:info "Agent is asigned newly generated agent-id: ~A"
                    (get-agent-id p))))))

(defun main (agent)
  (handler-case
      (as-singleton-agent
        (log:config :daily (log-file))
        ;; finish the agent initialization
        (setf (persistence agent) (init-persistence (persistence-file))
              (blobstore agent) (test-grid-gae-blobstore:make-blob-store
                                 :base-url
                                 ;; during development of GAE blob storage
                                 ;; :base-url may be "http://localhost:8080"
                                 "http://cl-test-grid.appspot.com"))
        (check-config agent)
        (ensure-has-id agent)
        ;; now do the work
        (let* ((quicklisp-version (update-testing-quicklisp (preferred-lisp agent)))
               (lib-world (format nil "quicklisp ~A" quicklisp-version)))
          (run-tests agent lib-world)))
    (serious-condition (c)
      (log:error "Unhandled seriours-condition of type ~A: ~A"
                 (type-of c) c))))

