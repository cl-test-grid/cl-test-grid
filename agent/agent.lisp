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
  ((persistence :type persistence :accessor persistence)
   (result-storage-name :type string
                        :accessor result-storage-name
                        :initarg :result-storage-name
                        :initform "main")
   (project-lister :type project-lister :accessor project-lister)
   ;; custom-lib-world may be a plist in the form
   ;; (:directory <pathname designator> :id <string>)
   (custom-lib-world :type list :accessor custom-lib-world :initform nil)
   (custom-project-names :type list :accessor custom-project-names :initform nil)))

;;; Now the LISPS property of agent may contain
;;; not only LISP-EXE as elements, but also
;;; lists in the form (LISP-EXE "storage1" ... "storageN")
;;;
;;; If the LISP-EXE is specified alone, it's test results
;;; will be submitted to the storage specified by the
;;; RESULTS-STORAGE-NAME agent property.
;;;
;;; If the LISP-EXE is specified together with storage
;;; names, then the results are submitted to these storages,
;;; and RESULTS-STORAGE-NAME is not used.
;;;
;;; Three functions below help to work with this specification.

(defun just-lisp-exe (lisp-spec)
  (typecase lisp-spec
    (list (car lisp-spec))
    (t lisp-spec)))

(defun lisp-exes (agent)
  (mapcar #'just-lisp-exe (lisps agent)))

(defun result-storages-for-lisp (agent lisp-exe)
  (let ((lisp-spec (find lisp-exe (lisps agent) :key #'just-lisp-exe)))
    (if (listp lisp-spec)
        (cdr lisp-spec)
        (list (result-storage-name agent)))))
;;; --- end of lisp specifications handling ---

(defmethod initialize-instance :after ((agent agent-impl) &rest initargs &key)
  (unless (slot-boundp agent 'work-dir)
    (setf (work-dir agent) (default-work-dir))))

(defmethod make-agent (&rest initargs)
  (apply #'make-instance 'agent-impl initargs))

;;; Working directory structure
(defun test-output-base-dir (agent)
  (merge-pathnames "test-runs/" (work-dir agent)))

;; File relative to the work-dir
(defun work-dir-child (agent relative-path)
  (merge-pathnames relative-path (work-dir agent)))

(defun log-file (agent)
  ;; good thing about log4cl, it creates
  ;; intermediate directories automatically,
  ;; so we need not care about this
  (work-dir-child agent "logs/agent.log"))

(defun quicklisp-update-log (agent)
  (work-dir-child agent "logs/quicklisp-update.log"))

(defun private-quicklisp-dir (agent)
  (if (custom-lib-world agent)
      (getf (custom-lib-world agent) :directory)
      (work-dir-child agent #P"quicklisp/")))

(defun persistence-file (agent)
  (work-dir-child agent "persistence.lisp"))

;; File relative to the src-dir
(defun src-file (file-name)
  (merge-pathnames file-name (src-dir)))

(defun projects-to-test (agent)
  (or (custom-project-names agent)
      (project-names (project-lister agent))))

(defun update-testing-quicklisp (agent dist-specifier)
  "Ensures the private quicklisp of the agent
is updated to the recent version. Returns
lib-world identifier of that quicklisp."
  (log:info "Ensuring the quicklisp used to download the libraries being tested is updated to the recent version...")
  (let ((quicklisp-version
         (with-response-file (response-file)
           (lisp-exe:run-with-timeout #.(* 2 60 60)
                                      (preferred-lisp agent)
                                      `(load ,(truename (src-file "proc-common.lisp")))
                                      `(load ,(truename (src-file "proc-update-quicklisp.lisp")))
                                      `(cl-user::set-response ,response-file
                                                              (cl-user::update-quicklisp ,(private-quicklisp-dir agent)
                                                                                         ,dist-specifier
                                                                                         ,(quicklisp-update-log agent)))))))
    (log:info "Quicklisp update process finished, current quicklisp version: ~A." quicklisp-version)
    quicklisp-version))

;;; Configuration check functions
(defun lisp-process-echo (lisp-exe str-to-echo)
  (with-response-file (response-file)
    (lisp-exe:run-with-timeout 60
                               lisp-exe
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
             (lisp-exe:run-with-timeout
              180
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
  (let ((failed-lisps
         (remove-if (alexandria:named-lambda bad-lisp-p (lisp)
                      (log:info "Checking external process functionality for ~A..." lisp)
                      (handler-case (progn
                                      (check-lisp lisp)
                                      (log:info "~A OK" lisp)
                                      nil)
                        (serious-condition (c)
                          (log:error "Lisp failed: ~A ~A~%" lisp c)
                          t)))
                    (lisp-exes agent))))
    (if (null failed-lisps)
        (log:info "All the external lisps passed the configuration check OK")
        (error "Failed lisps: ~S"  failed-lisps)))
  t)

(defparameter +implementation-identifier-timeout+ (* 3 60))

(defun implementation-identifier (lisp-exe)
  (let ((identifier
         (with-response-file (response-file)
           (lisp-exe:run-with-timeout +implementation-identifier-timeout+
                                      lisp-exe
                                      `(load ,(truename (src-file "proc-common.lisp")))
                                      `(load ,(truename (src-file "proc-implementation-identifier.lisp")))
                                      `(cl-user::set-response ,response-file
                                                              (funcall (read-from-string
                                                                        "implementation-identifier::implementation-identifier")))))))
    ;; For ECL append type of compiler (bytecode or lisp-to-c)
    ;; to the implementation identifier.
    ;; Note, this can not be done by making
    ;; the IMPLEMENTATION-IDENTIFIER a generic
    ;; function and defining special method for ECL,
    ;; because we memoize IMPLEMENTAION-IDENTIFIER function,
    ;; and there is no portable way to memoize
    ;; generic functions.
    (cond ((typep lisp-exe 'lisp-exe:ecl)
           (format nil "~A-~A"
                   identifier
                   (string-downcase (lisp-exe:compiler lisp-exe))))
          ((typep lisp-exe 'lisp-exe:clisp-asdf3)
           (format nil "~A-asdf3" identifier))
          (t identifier))))

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
    (values (reverse positive) (reverse negative))))

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
  (divide-into (lisp-exes agent) (lambda (lisp)
                                   (tested-p (persistence agent)
                                             lib-world
                                             (implementation-identifier lisp)))
      (done-lisps pending-lisps)
    (when done-lisps
      (log:info "Skipping the lisps already tested on ~A: ~S"
                lib-world
                (mapcar #'implementation-identifier done-lisps)))
    (let ((unfinished-test-runs :load-me-lazy))
      (flet ((find-unfinished-run (lib-world lisp-implementation-identifier)
               (when (eq :load-me-lazy unfinished-test-runs)
                 (setf unfinished-test-runs (list-unfinished-test-runs (test-output-base-dir agent))))
               (find-if (lambda (unfinished-test-run)
                          (and (string= lib-world (getf (test-grid-data::run-descr unfinished-test-run)
                                                        :lib-world))
                               (string= lisp-implementation-identifier
                                        (getf (test-grid-data::run-descr unfinished-test-run)
                                              :lisp))))
                        unfinished-test-runs))
             (make-run (lib-world lisp-implementation-identifier)
               (list :descr
                     (list :lisp lisp-implementation-identifier
                           :lib-world lib-world
                           :time (get-universal-time)
                           :run-duration :unknown
                           :contact-email (user-email agent)))))
        (dolist (lisp pending-lisps)
          (handler-bind
              ((serious-condition (lambda (c)
                                    (let ((bt (with-output-to-string (s)
                                                (trivial-backtrace:print-backtrace-to-stream s))))
                                      (log:error "Error of type ~A during tests on ~A: ~A~%~A~%Continuing for the remaining lisps."
                                                 (type-of c) (implementation-identifier lisp) c bt))
                                    (go continue))))
            (log:info "Running tests for ~A" (implementation-identifier lisp))
            (let ((results-dir (complete-test-run (or (find-unfinished-run lib-world (implementation-identifier lisp))
                                                      (make-run lib-world (implementation-identifier lisp)))
                                                  agent
                                                  lisp
                                                  (projects-to-test agent))))
              (submit-test-run-results results-dir (result-storages-for-lisp agent lisp))
              (mark-tested (persistence agent) lib-world (implementation-identifier lisp))
              (cl-fad:delete-directory-and-files results-dir :if-does-not-exist :ignore)))
          continue)))))

(defun ensure-has-id (agent)
  (let* ((p (persistence agent))
         (id (get-agent-id p)))
    (if id
        (log:info "agent-id: ~A" id)
        (progn
          (set-agent-id p (generate-id))
          (log:info "Agent is asigned newly generated agent-id: ~A"
                    (get-agent-id p))))))

(defun send-hello-notification (agent)
  (log:info "sending hello notification")
  (send-notification (format nil "[agent hello] from ~A (~A)"
                             (get-agent-id (persistence agent))
                             (user-email agent))
                     (format nil "~(~S~)"
                             (list :user-email (user-email agent)
                                   :agent-id (get-agent-id (persistence agent))
                                   :lisp-exes (mapcar #'implementation-identifier
                                                      (lisp-exes agent))
                                   :preferred-lisp (implementation-identifier (preferred-lisp agent))))))
(defmethod main (agent
                 ;; QL-DOST is NOT a part of the public API (yet).
                 ;; The value may be either "quicklisp", or "qlalpha",
                 ;; or an URL, like
                 ;; "http://beta.quicklisp.org/dist/quicklisp/2012-08-11/distinfo.txt"
                 &key (ql-dist "quicklisp"))
  ;; setup logging for unhandled errors and warnings
  (handler-bind
      ((serious-condition (lambda (c)
                            (let ((bt (with-output-to-string (s)
                                        (trivial-backtrace:print-backtrace-to-stream s))))
                              (log:error "Unhandled seriours-condition of type ~A: ~A~%~A"
                                         (type-of c) c bt))
                            (return-from main)))
       (warning (lambda (w)
                  (log:warn "A warning is signalled: ~A" w)
                  (muffle-warning))))
    (as-singleton (agent)
      (log:config :daily (namestring (log-file agent)) :immediate-flush)
      (let ((lisp-exe:*temp-dir* (work-dir agent)))
        ;; finish the agent initialization
        (setf (persistence agent) (init-persistence (persistence-file agent)))
        (check-config agent)
        (ensure-has-id agent)
        (send-hello-notification agent)
        (let ((lib-world (or (getf (custom-lib-world agent) :id)
                             (update-testing-quicklisp agent ql-dist))))
          (setf (project-lister agent)
                (init-project-lister (preferred-lisp agent)
                                     (private-quicklisp-dir agent)
                                     ql-dist))
          ;; now do the work
          (run-tests agent lib-world)))))
  (log:info "Agent is done. Bye."))


;;; test-patched-quicklisp

;; with-value: is there a standard tool for this?
(defmacro with-value ((place value) &body body)
  (let ((old (gensym "old-value-")))
    `(let ((,old ,place))
       (unwind-protect
            (progn
              (setf ,place ,value)
              ,@body)
         (setf ,place ,old)))))

(defun test-patched-quicklisp (agent quicklisp-dir lib-world-id &key project-names)
  (with-value ((custom-lib-world agent)
               (list :directory quicklisp-dir :id lib-world-id))
    (with-value ((custom-project-names agent) project-names)
      (main agent))))
