;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

(defclass agent () 
   ;; The list of lisp-exe's to run tests on.
  ((lisp-exes :type list :accessor lisp-exes :initform nil)
   ;; The lisp-exe considered as more reliable on this OS, 
   ;; and supporting more libraries. Used run various small
   ;; lisp programs like quicklisp update.
   (preferred-lisp-exe :type (or null lisp-exe) :accessor preferred-lisp-exe :initform nil)
   (user-email :type (or null string) :accessor user-email :initform nil)
   ;; ------ package private ------
   ;; todo: make private (i.e. export other symbols form the package
   ;; but not export private symbol). cl-test-grid-config.lisp
   ;; should work via the exported symbols.
   (persistent-state :type list :accessor persistent-state)
   ;; list of lisp-exe-ex corresponging to the lisp-exes
   (lisps :type list :accessor lisps)
   (preferred-lisp :type lisp-exe-ex :accessor preferred-lisp)))

(defmethod (setf lisp-exes) :after (new-lisp-exes (agent agent))
  (setf (lisps agent)
        (mapcar (lambda (lisp-exe) 
                  (make-instance 'lisp-exe-ex :exe lisp-exe))
                new-lisp-exes)))

(defmethod (setf preferred-lisp-exe) :after (new-lisp-exe (agent agent))
  (setf (preferred-lisp agent) (make-instance 'lisp-exe-ex :exe new-lisp-exe)))

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

(defun mark-tested (agent &optional lib-world lisp libname-or-list)
  (let ((libnames (ensure-list libname-or-list)))
    (dolist (libname libnames)
      (setf (persistent-state agent)
            (agent-state:mark-done (persistent-state agent) 
                                   lib-world 
                                   lisp
                                   libname)))
    (save-state (persistent-state agent))))

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

(defun workdir-file (relative-path)
  (merge-pathnames relative-path (work-dir)))

;;; File relative to the src-dir
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
;; - configuration: configure list of lisps, user email. 
;; - configuration: test-config procedcure (try to run all the lisps configured)
;; - configuration: contributor email as a configurabel agent propery,
;;   instead of cl-test-grid-settings.lisp
;; - delete test run data after the results have been submitted
;; * Level 1 stable
;; + full path to files: setup, test-run-process, response-file
;; - timeout
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

;;; When we run lisp code in an external lisp process,
;;; and want to return some value form that process.
;;; We use a temporary file where the external process
;;; stores the response and we READ that response
;;; from the file.
(defun with-response-file-impl (body-func)
  (let* ((response-file-name (format nil
                                     "response~A.lisp"
                                     (random #.(1- (expt 2 64)))))
         (response-file (workdir-file response-file-name)))
    (unwind-protect (progn (funcall body-func response-file)
                           (test-grid::safe-read-file response-file))
      (when (probe-file response-file)
        (delete-file response-file)))))

(defmacro with-response-file ((file-var) &body body)
  `(with-response-file-impl (lambda (,file-var) ,@body)))

;;; Primary functions of test-grid agent
(defun run-tests-in-separate-process (agent lisp-exe test-suites)
  "Runs the specified test suites in a separate process
and returns the directory where the test results are saved."
  (with-response-file (response-file)
    (let* ((code `(progn
                    (load ,(workdir-file "quicklisp/setup.lisp"))
                    (load ,(src-file "test-run-process.lisp"))
                    (cl-user::run-with-response-to-file (quote ,test-suites)
                                                        ,(test-output-base-dir)
                                                        ,(user-email agent)
                                                        ,response-file))))
      (log:info "preparing to start separate lisp process with code: ~S" code)
      (run-lisp-process lisp-exe code))))

(defun update-testing-quicklisp (lisp-exe)
  (log:info "Ensuring the quicklisp used to download the libraries being tested is updated to the recent version...")
  (let ((quicklisp-version
         (with-response-file (response-file)
           (run-lisp-process lisp-exe
                             `(progn 
                                (load ,(truename (src-file "ensure-quicklisp-updated.lisp")))
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
    (run-lisp-process lisp-exe 
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

;;; split a list into sublists by n elements,
;;; e.g. (a b c d e) by 3 => (a b c) (d e)
(defclass list-splitter ()
  ((remainder :type list 
              :accessor remainder
              :initarg :list
              :initform (error ":list is required"))))

(defun next (list-splitter &optional (n 1))
  (let ((result nil))
    (dotimes (i n)
      (when (null (remainder list-splitter))
        (return-from next (nreverse result)))
      (push (car (remainder list-splitter)) result)
      (setf (remainder list-splitter)
            (cdr (remainder list-splitter))))
    (nreverse result)))

(let ((ls (make-instance 'list-splitter :list '(1 2 3))))
  (assert (equal '(1 2) (next ls 2)))
  (assert (equal '(3) (next ls 2)))
  (assert (equal nil (next ls 2))))

(defun split-list (list n)
  (let ((splitter (make-instance 'list-splitter :list list))
        (result nil))
    (loop 
       (let ((sub (next splitter n)))
         (when (null sub)
           (return (nreverse result)))
         (push sub result)))))

(assert (equal '((1 2 3) (4 5))
               (split-list '(1 2 3 4 5) 3)))

(defun lisp-name (lisp-exe)
  (let ((ql-setup-file (workdir-file "quicklisp/setup.lisp")))
    (when (not (probe-file ql-setup-file))
      (error "Can not determine lisp implemntation name until quicklisp is installed - we need ASDF installed together with quicklisp to evaluate (asdf::implementation-identifier)."))
    (with-response-file (response-file)
      (run-lisp-process lisp-exe 
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
                                                ;; it can't read it.
                                                (funcall (read-from-string 
                                                          "asdf::implementation-identifier")))))))

(defclass lisp-exe-ex ()
  ((exe :type lisp-exe :accessor exe :initarg :exe :initform (error ":exe is a required argument"))
   (implementation-identifier :type (or null string) :initarg :name :initform nil))
  (:documentation "Wraps lisp-exe and stores (cached) attributes needed by agent."))

;; For convinience, define a method for run-lisp-process on
;; lisp-exe-ex
(defmethod run-lisp-process ((lisp-exe-ex lisp-exe-ex) &rest forms)
  (apply #'run-lisp-process (exe lisp-exe-ex) forms))

(defun implementation-identifier (lisp-exe-ex)
  (with-slots (implementation-identifier) lisp-exe-ex
    (when (not implementation-identifier)
      (setf implementation-identifier
            (lisp-name lisp-exe-ex)))
    implementation-identifier))

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
      (log:info "Running tests for ~A" (implementation-identifier lisp))  
      (divide-into test-grid::*all-libs* (lambda (libname)
                                           (tested-p agent
                                                     lib-world
                                                     (implementation-identifier lisp)
                                                     libname))
          (done-libs pending-libs)
        (when done-libs
          (log:info "Skipping the libraries already tested on ~A and ~A: ~S"
                    lib-world
                    (implementation-identifier lisp)
                    done-libs))
        (dolist (lib-block (split-list pending-libs 8))
          (log:info "libraries block to test in a single process: ~S" lib-block)
          (let ((results-dir (run-tests-in-separate-process agent lisp lib-block)))
            (test-grid::submit-results results-dir)
            (mark-tested agent lib-world lisp lib-block)
            (cl-fad:delete-directory-and-files results-dir :if-does-not-exist :ignore)))
        (mark-tested agent lib-world (implementation-identifier lisp))))
    (mark-tested agent lib-world)))

(defun load-agent ()
  (let ((agent (make-instance 'agent)))
    (load-config agent)
    (setf (persistent-state agent) (load-state))
    agent))

(defun main ()
  (log:config :daily (log-file))
  (let ((*agent* (load-agent)))
    (check-config *agent*)
    (let ((quicklisp-version (update-testing-quicklisp (preferred-lisp *agent*))))
      (run-tests *agent* (format nil "quicklisp ~A" quicklisp-version)))))

(main)
;(update-testing-quicklisp *ccl-18-32*)

;;(run-tests-in-separate-process *acl* (list :babel))


;;  (time
;;   (dolist (test-suite test-grid::*all-libs*)
;;     (run-tests-in-separate-process *acl* (list test-suite))))

;; ===================================

;; (external-program:run "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl.exe"
;;                       '("--no-init"
;;                         "--load" "quicklisp/setup.lisp"
;;                         "--load" "run-agent.lisp"
;;                         "--eval" "\"(test-grid::run-libtests '(:babel))\""
;;                         "--eval" "(quit)"))

;; (external-program:run "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl.exe"
;;                       '("--no-init"
;;                         "--load" "quicklisp/setup.lisp"
;;                         "--load" "run-agent.lisp"
;;                         "--eval" "(test-grid::run-libtests '(:iterate))"
;;                         "--eval" "(quit)"))

