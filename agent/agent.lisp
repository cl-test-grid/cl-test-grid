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
   ;; todo: make private (i.e. export other symbols form the package
   ;; but not export private symbol). cl-test-grid-config.lisp
   ;; should work via the exported symbols.
   (persistent-state :type list :accessor persistent-state)
   (blobstore :accessor blobstore)))

;;; High level presistence functions

(defun persistence-file ()
  (workdir-file "persistence.lisp"))

(defun load-state ()
  (if (not (probe-file (persistence-file)))
      (agent-state:make-state)
      (test-grid::safe-read-file (persistence-file))))

(defun save-state (state)
  ;; todo: sort and newline for each record
  (test-grid::write-to-file state (persistence-file)))

(defun tested-p (agent lib-world &optional lisp libname)
  (agent-state:done-p (persistent-state agent)
                      lib-world
                      lisp
                      libname))

;; helper function
(defun ensure-list (arg)
  (if (listp arg) arg (list arg)))

(defun mark-tested (agent lib-world &optional lisp libname-or-list)
  (flet ((mark (lib-world lisp &optional libname)
           (setf (persistent-state agent)
                 (agent-state:mark-done (persistent-state agent)
                                        lib-world
                                        lisp
                                        libname))))
    (if libname-or-list
        ;; some libraries specified, do loop
        (let ((libnames (ensure-list libname-or-list)))
          (dolist (libname libnames)
            (mark lib-world lisp libname)))
        ;; libraries are omitted (only lib-world and maybe lisp are specified);
        ;; the above loop will not call MARK at all in this case, therfore
        ;; we need a separate IF branch to ensure we saved the information
        (mark lib-world lisp)))
  (save-state (persistent-state agent)))

;;; File system roots:
(defun work-dir ()
  (merge-pathnames "work-dir/agent/"
                   test-grid-config::*src-base-dir*))

(defun src-dir ()
  "File system location of test-grid-agent source code"
  (merge-pathnames "agent/"
                   test-grid-config::*src-base-dir*))

(defun config-dir ()
  (user-homedir-pathname))

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

;; File relative to the src-dir
(defun src-file (file-name)
  (merge-pathnames file-name (src-dir)))

;;; Config file
(defparameter +config-file-name+ "cl-test-grid-config.lisp")

(defun config-file()
  (merge-pathnames (config-dir) +config-file-name+))


;; sketch of the procedure to implement:
;; Level 1
;; - ensure quicklisp is updated to the lates version
;; - run the tests and upload results
;; Level 2
;; - ensure quicklisp is updated to the version we need to test
;; - ask each lisp for it's name
;; - retrieve (either from the local presistence or from server)
;;   the list of libraries which have not beeen tested
;;   on this quicklisp and on our lisps
;; - run the tests and upload results (and mark the
;;   libraries as being tested so that we do not
;;   repeat the tests)

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
;; - check status of the quicklisp update process
;; - Prevent lisp entering debugger.
;; - Different lisps treat unhangled signals during -eval
;;   differently: ECL exits with status 1, CCL enters debugger
;;   and hangs.
;; - When signalling absense of the config file, provide
;;   a fully commented example config file so that user can easily
;;   fix it.
;; - In the config file, should user work in the test-grid-agent
;;   package, or in cl-user package and use a public API of
;;   test-grid-agent?
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
;; * Very stable
;; - ECL needs C compiler, how to ensure it is available (can we specify
;;   it explicitly when configurin lisp-exe for ECL or assume
;;   it is available in path)?
;; * Backward compatibility
;; - remove the old test-runs directory
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
                                      `(progn
                                         (load ,(truename (src-file "proc-common.lisp")))
                                         (load ,(truename (src-file "proc-update-quicklisp.lisp")))
                                         (with-open-file (cl-user::out ,response-file
                                                                       :direction :output
                                                                       :if-exists :supersede
                                                                       :if-does-not-exist :create)
                                           (pprint (cl-user::do-quicklisp-update) cl-user::out)))))))
    (log:info "Quicklisp update process finished, current quicklisp version: ~A." quicklisp-version)
    quicklisp-version))

(defparameter *agent* nil
  "The AGENT instance. This variable is provided in order
to make the agent accessible from the user-edited config
file cl-test-grid-config.lisp.")

(defun load-config (agent)
  (let ((config-file (config-file)))
    (unless (probe-file config-file)
      (error "Configuration file ~A is absent, can not run test-grid agent." config-file))
    (let ((*package* (find-package '#:test-grid-agent))
          (*agent* agent))
      (load config-file))))

;;; Configuration check functions
(defun lisp-process-echo (lisp-exe str-to-echo)
  (with-response-file (response-file)
    (lisp-exe:run-lisp-process lisp-exe
                               `(with-open-file (cl-user::out ,response-file
                                                              :direction :output
                                                              :if-exists :supersede
                                                              :if-does-not-exist :create)
                                  (pprint ,str-to-echo cl-user::out)))))

(defun check-lisp (lisp-exe)
  (let* ((echo-string "abcs *() { /d e11 ")
         (response (lisp-process-echo lisp-exe echo-string)))
    (when (not (string= echo-string response))
      (error "Lisp ~A returned string ~S while we exected ~S."
             lisp-exe response echo-string))))

(defun check-config (agent)
  (log:info "Checking configuration...")
  (when (zerop (length (user-email agent)))
    (error "The user-email property of test-grid:*agent* can not be empty. Please specify your email, or at least some nickname if you are strongly opposed to publish your email."))
  (log:info "Checking external process functionality for the preffered lisp ~A..."
            (preferred-lisp agent))
  (check-lisp (preferred-lisp agent))
  (dolist (lisp (lisps agent))
    (log:info "Checking external process functionality for ~A..." lisp)
    (check-lisp lisp)
    (log:info "~A OK" lisp))
  (log:info "All the external lisps passed the configuration check OK")
  t)

(defun implementation-identifier (lisp-exe)
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
                                                                   "asdf::implementation-identifier")))))))
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
  (when (tested-p agent lib-world)
    (log:info "~A is already fully tested. Skipping." lib-world)
    (return-from run-tests))

  (divide-into (lisps agent) (lambda (lisp)
                               (tested-p agent
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
              (mark-tested agent lib-world (implementation-identifier lisp))
              (cl-fad:delete-directory-and-files results-dir :if-does-not-exist :ignore))
            (mark-tested agent lib-world (implementation-identifier lisp)))
        (serious-condition (e)
          (log:error "Error during tests on ~A. Continuing for remaining lisps."
                     (implementation-identifier lisp))))))

  ;; do not mark the whole lib-world as :done, because I am experimenting with different lisps
  ;; and want them to run tests next time when agent is started
  ;;(mark-tested agent lib-world)
  )

(defun load-agent ()
  (let ((agent (make-instance 'agent)))
    (load-config agent)
    (setf (persistent-state agent) (load-state)
          (blobstore agent)
          ;; during development of GAE blob storage :base-url may be "http://localhost:8080"
          (test-grid-gae-blobstore:make-blob-store :base-url "http://cl-test-grid.appspot.com"))
    agent))

(defun main ()
  (log:config :daily (log-file))
  (let ((*agent* (load-agent)))
    (check-config *agent*)
    (let ((quicklisp-version (update-testing-quicklisp (preferred-lisp *agent*))))
      (run-tests *agent* (format nil "quicklisp ~A" quicklisp-version)))))
