;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

;;; File system roots:

(defun src-dir()
  (asdf:system-relative-pathname :test-grid-agent #P"agent/"))

(defun src-super-root()
  (merge-pathnames "../" (src-dir)))

(defun default-work-dir ()
  (merge-pathnames "work-dir/agent/" (src-super-root)))

;;; Agent implementation class
(defclass agent-impl (agent)
  ( ;; initial value for the base class slots
   (work-dir :initform (default-work-dir))
   ;; own slots
   (persistence :type persistence :accessor persistence)
   (blobstore :accessor blobstore)))

(defmethod make-agent ()
  (make-instance 'agent-impl))

;;; Working directory structure
(defun test-output-base-dir (agent)
  (merge-pathnames "test-runs/" (work-dir agent)))

(defun log-file (agent)
  ;; good thing about log4cl, it creates
  ;; intermediate directories automatically,
  ;; so we need not care about this
  (merge-pathnames "logs/agent.log"
                   (work-dir agent)))

;; File relative to the work-dir
(defun workdir-child (agent relative-path)
  (merge-pathnames relative-path (work-dir agent)))

(defun private-quicklisp-dir (agent)
  (workdir-child agent #P"quicklisp/"))

(defun persistence-file (agent)
  (workdir-child agent "persistence.lisp"))

;; File relative to the src-dir
(defun src-file (file-name)
  (merge-pathnames file-name (src-dir)))

(defun update-testing-quicklisp (agent)
  (log:info "Ensuring the quicklisp used to download the libraries being tested is updated to the recent version...")
  (let ((quicklisp-version
         (with-response-file (response-file)
           (lisp-exe:run-lisp-process (preferred-lisp agent)
                                      `(load ,(truename (src-file "proc-common.lisp")))
                                      `(load ,(truename (src-file "proc-update-quicklisp.lisp")))
                                      `(cl-user::set-response ,response-file
                                                              (cl-user::do-quicklisp-update ,(private-quicklisp-dir agent)))))))
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
           ;; on the printed representation of the compiled
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

(defparameter +implementation-identifier-timeout+ (* 3 60))

(defgeneric implementation-identifier (lisp-exe)
  (:method ((lisp-exe lisp-exe:lisp-exe))
    (with-response-file (response-file)
      (lisp-exe:run-with-timeout +implementation-identifier-timeout+
                                 lisp-exe
                                 `(load ,(truename (src-file "proc-common.lisp")))
                                 `(load ,(truename (src-file "proc-implementation-identifier.lisp")))
                                 `(cl-user::set-response ,response-file
                                                         (funcall (read-from-string
                                                                   "implementation-identifier::implementation-identifier")))))))
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
      (handler-bind
          ((serious-condition (lambda (c)
                                (let ((msg (with-output-to-string (s)
                                             (format s
                                                     "Error of type ~A during tests on ~A: ~A. Continuing for remaining lisps."
                                                     (type-of c) (implementation-identifier lisp) c)
                                             (trivial-backtrace:print-backtrace-to-stream s))))
                                  (log:error (log:make-logger) msg)
                                  (go continue)))))
        (log:info "Running tests for ~A" (implementation-identifier lisp))
        (let ((results-dir (perform-test-run agent
                                             lib-world
                                             lisp
                                             test-grid-testsuites::*all-libs*)))
          (submit-test-run-results (blobstore agent) results-dir)
          (mark-tested (persistence agent) lib-world (implementation-identifier lisp))
          (cl-fad:delete-directory-and-files results-dir :if-does-not-exist :ignore)))
      continue)))

(defun ensure-has-id (agent)
  (let* ((p (persistence agent))
         (id (get-agent-id p)))
    (if id
        (log:info "agent-id: ~A" id)
        (progn
          (set-agent-id p (generate-id))
          (log:info "Agent is asigned newly generated agent-id: ~A"
                    (get-agent-id p))))))

(defun say-hello-to-admin (agent)
  (log:info "sending hello message to admin")
  (test-grid-blobstore:tell-admin (blobstore agent)
                                  (format nil "[agent hello] from ~A (~A)"
                                          (get-agent-id (persistence agent))
                                          (user-email agent))
                                  "hello"))

(defmethod main (agent)
  (handler-bind
      ((serious-condition (lambda (c)
                            (let ((msg (with-output-to-string (s)
                                         (format s "Unhandled seriours-condition of type ~A: ~A"
                                                 (type-of c) c)
                                         (trivial-backtrace:print-backtrace-to-stream s))))
                              (log:error (log:make-logger) msg)
                              (return-from main)))))
    (as-singleton-agent
      (let ((*response-file-temp-dir* (work-dir agent)))
        (error "hru-hru")
        (log:config :daily (log-file agent) :immediate-flush)
        ;; finish the agent initialization
        (setf (persistence agent) (init-persistence (persistence-file agent))
              (blobstore agent) (test-grid-gae-blobstore:make-blob-store
                                 :base-url
                                 ;; during development of GAE blob storage
                                 ;; :base-url may be "http://localhost:8080"
                                 "http://cl-test-grid.appspot.com"))
        (check-config agent)
        (ensure-has-id agent)
        (say-hello-to-admin agent)
        ;; now do the work
        (let* ((quicklisp-version (update-testing-quicklisp agent))
               (lib-world (format nil "quicklisp ~A" quicklisp-version)))
          (run-tests agent lib-world))))))
