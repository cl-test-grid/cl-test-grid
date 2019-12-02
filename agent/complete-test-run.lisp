;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

#|

The complete-test-run function defined in this file has several variants
for different parameters. Example for complete-test-run2:

(defparameter *sbcl*
  (make-instance 'lisp-exe:sbcl
                 :exe-path "C:\\Program Files\\Steel Bank Common Lisp\\1.1.0.36.mswinmt.1201-284e340\\run.bat"))

(tg-agent::complete-test-run2 '(:lib-world "quicklisp 2013-01-28"
                                :contact-email "avodonosov@yandex.ru")
                              (merge-pathnames "test-runs/my-test-run/" (user-homedir-pathname))
                              (merge-pathnames "quicklisp/" (user-homedir-pathname))
                              "qlalpha"
                              *sbcl*)

The first parameter is the plist of test run description attributes.
You must specify at least :lib-world and :contact-email. All other
test run description attributes will be computed automatically.

The second parameter is the directory where test results will be placed.

The third parameter is the directory where Quicklisp to be tested is located.

The fourth parameter is the name of the active Quicklisp dist - we will be
collecting project names and system names from that dist, and expect
ql:quickload will use that dist (i.e. you have already switched to that
dist if necessary).

The fifth parameter is the lisp-exe instance to be tested.

The function also has keyword parameter :project-names allowing
to specify explicitly list of projects, instead of testing all the
projects in quicklisp.

Keyword aprameter :helper-lisp-exe allows to specify a lisp-exe used
to perform auxilary-tasks (like retrieving mapping from projects to their
asdf systems).

Example:

(defparameter *ccl*
  (make-instance 'lisp-exe:ccl
                 :exe-path "/home/anton//unpacked/ccl-1.8/lx86cl64"))


(tg-agent::complete-test-run2 '(:lib-world "quicklisp 2013-01-28"
                                :contact-email "avodonosov@yandex.ru")
                              (merge-pathnames "test-runs/my-test-run/" (user-homedir-pathname))
                              (merge-pathnames "quicklisp/" (user-homedir-pathname))
                              "quicklisp"
                              *ccl*
                              :project-names '(:alexandria :babel :flexi-streams)
                              :helper-lisp-exe *sbcl*)

|#

(in-package #:test-grid-agent)

(defun make-run-descr (lib-world lisp-implementation-identifier user-email)
  "Generate a description for a test run which might be
performed in the current lisp system."
  (list :lisp lisp-implementation-identifier
        :lib-world lib-world
        :time (get-universal-time)
        :run-duration :unknown
        :contact-email user-email))

(defun fmt-time (universal-time &optional destination)
  "The preferred time format used in the cl-test-grid project."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time 0)
    (format destination
            "~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
            year month date hour min sec)))

(defun os-windows-p ()
  ;; :windows feature is put into *features* by trivial-features
  (member :windows *features*))

(defun implementation-type (impl-identifier)
  (subseq impl-identifier 0 (position #\- impl-identifier)))

(defun hash-char (str)
  (let ((hash-str (format nil "~36,r"
                          (reduce #'+ (flexi-streams:string-to-octets str
                                                                      :external-format :utf-8)))))
    (char-downcase (aref hash-str (1- (length hash-str))))))

(defun name-run-directory (run-descr)
  "Generate name for the directory where test run
data (libraries test suites output and the run results) will be saved."
  (if (os-windows-p)
      ;; On windows shorten the directory name by using only implementation type
      ;; insted of full implementation identifier. Shorter name helps
      ;; to prevent problems with too long .fasl file names,
      ;; because windows has max path length of 256 chars.
      (format nil
              "~A.~(~A~)-~A"
              (fmt-time (getf run-descr :time))
              (hash-char (getf run-descr :lisp)) ;; add hash-char to make it more unique
              (implementation-type (getf run-descr :lisp)))
      (format nil
              "~A-~A"
              (fmt-time (getf run-descr :time))
              (getf run-descr :lisp))))

(defun run-directory (run-descr base-dir)
  (merge-pathnames (make-pathname
                    :directory (list :relative (name-run-directory run-descr))
                    :name      nil
                    :type      nil)
                   base-dir))

(defun make-run (description lib-results)
  (list :descr description :results lib-results))

(defun run-info-file (test-run-directory)
  (merge-pathnames "test-run-info.lisp"
                   test-run-directory))

(defun save-run-info (test-run directory)
  (let ((run-file (run-info-file directory)))
    (with-open-file (out run-file
                         :direction :output
                         :element-type 'character
                         :external-format tg-utils:*utf-8-external-format*
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (test-grid-data::print-test-run out test-run))))

(defun test-run-log-dir (run-directory)
  (merge-pathnames "logs/" run-directory))

(defun lib-log-name (lib-name)
  (substitute #\- #\.
              ;; Substitute dots by hypens because CCL
              ;; prepends the > symbol before dots (at least on windows);
              ;; for example: hu.dwim.stefil => hu>.dwim.stefil.
              ;; When we pass such a pathname to another lisps,
              ;; they can't handle it.
              (string-downcase lib-name)))

(defun lib-log-file (test-run-directory lib-name)
  (merge-pathnames (lib-log-name lib-name)
                   (test-run-log-dir test-run-directory)))

(defparameter +libtest-timeout-seconds+ #.(* 15 60)
  "Maximum number of seconds we give each library test suite
to complete. After this time we consider the test suite
as hung, kill the lisp process and record :TIMEOUT
as the library test result.")

(defparameter +loadtest-timeout-seconds+ #.(* 15 60)
  "Maximum number of seconds we give each ASDF system to load
in a fresh lisp process.  After this time we consider system
as hung, kill the lisp process and record :TIMEOUT
as the system load status.")

(defun pretty-fmt-time (universal-time &optional destination)
  "The human-readable time format, used in reports."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time 0)
    (format destination
            "~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour min sec)))

(defmacro appending (stream-var file &body body)
  `(with-open-file (,stream-var ,file
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :append
                                :element-type 'character
                                :external-format tg-utils:*utf-8-external-format*)
     ,@body))

(defun print-testsuite-log-header (libname run-descr log-file)
  (with-open-file (stream (ensure-directories-exist log-file)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type 'character
                          :external-format tg-utils:*utf-8-external-format*)
    (let ((*print-case* :downcase) (*print-pretty* nil))
      (format stream "============================================================~%")
      (format stream "  cl-test-grid testsuite execution~%")
      (format stream "------------------------------------------------------------~%")
      (format stream "  library:           ~A~%" libname)
      (format stream "  lib-world:         ~A~%" (getf run-descr :lib-world))
      (format stream "  lisp:              ~A~%" (getf run-descr :lisp))
      (format stream "  contributor email: ~A~%" (getf run-descr :contact-email))
      (format stream "  timestamp:         ~A~%" (pretty-fmt-time (get-universal-time))))))

(defun print-log-footer (lib-or-system status log-file)
  (appending stream log-file
    (let ((*print-case* :downcase))
      (fresh-line stream)
      (terpri stream)
      (format stream "~%============================================================~%")
      (format stream "  cl-test-grid status for ~A: ~A~%"
              lib-or-system (test-grid-data::print-test-status nil status))
      (format stream "============================================================~%"))))

(defun restarting-on-hibernate-impl (body-fn)
  "Runs the BODY-FN function. If during the BODY-FN
execution a LIXP-EXE:HIBERNATION-DETECTED condition
is signalled, then upon BODY-FN completion runs it again."
  (let (retry)
    (handler-bind ((lisp-exe:hibernation-detected
                    (lambda (condition)
                      (declare (ignore condition))
                      (log:warn "Hibernation detected. After the process is finshed, we will re-run it again to rule out possible errors caused by the hibernation, like loosing network connections and similar. (Not killing the process immediatelly to avoid inconsistencies left by the process, like half-written .fasl files)")
                      (setf retry t))))
      (loop
         (setf retry nil)
         (let ((result (multiple-value-list (funcall body-fn))))
           (if (not retry)
               (return (values-list result))
               (log:info "Re-running the process:")))))))

(defmacro restarting-on-hibernate (&body body)
  "Runs the BODY. If during the BODY execution a
LIXP-EXE:HIBERNATION-DETECTED condition is signalled, then
upon the BODY completion runs the BODY again."
  `(restarting-on-hibernate-impl (lambda () ,@body)))

(defun ensure-list-response (response)
  (if (listp response)
      response
      (progn
        (log:info "The response is expected to be a LIST, but was a ~A: ~S. We consider the lisp process crashed. "
                  (type-of response) response)
        '(:status :crash))))

(defun proc-run-libtest (lisp-exe libname run-descr logfile private-quicklisp-dir asdf-output-dir &key eval-before-test)
  "Runs test-grid-testsuites::run-libtest in a separate process and returns the result."
  (restarting-on-hibernate
    (let ((start-time (get-internal-real-time))
          (status (handler-case
                      (ensure-list-response
                       (with-response-file (response-file)
                         (let* ((code `(progn
                                         (load ,(merge-pathnames "setup.lisp" private-quicklisp-dir))
                                         (load ,(src-file "proc-common.lisp"))
                                         (load ,(src-file "proc-common-asdf.lisp"))
                                         (load ,(src-file "proc-run-libtest.lisp"))
                                         (cl-user::set-response ,response-file
                                                                (cl-user::run-libtest-main ,libname
                                                                                           ,logfile
                                                                                           ,private-quicklisp-dir
                                                                                           ,asdf-output-dir
                                                                                           :eval-before-test ',eval-before-test)))))
                           (log:info "Starting ~A test suite..." libname)
                           (print-testsuite-log-header libname run-descr logfile)
                           (lisp-exe:run-with-timeout +libtest-timeout-seconds+ lisp-exe code))))
                    (no-response (condition)
                      (appending log logfile
                        (format log "~%Child lisp process running the ~A test suite finished without returing result test status. "
                                libname)
                        (format log "Looks like the lisp process has crashed. ")
                        (format log "The error condition signalled in the parent process: ~A~%" condition))
                      (log:info "Child lisp process seems crashed: didn't returned any response. The NO-RESPONSE error signalled: ~A"
                                condition)
                      '(:status :crash))
                    (lisp-exe:lisp-process-timeout (c)
                      (appending log logfile
                        (format log "~%The ~A test suite hasn't finished in ~A seconds." libname (lisp-exe:seconds c))
                        (format log "~%We consider the test suite as hung; the test suite lisp process is killed.~%"))
                      (log:info "Child lisp process running ~A test suite has exceeded the timeout of ~A seconds and killed."
                                libname (lisp-exe:seconds c))
                      '(:status :timeout)))))
      (log:info "The ~A test suite result: ~S" libname status)
      (print-log-footer libname (getf status :status) logfile)
      (append (list :libname libname)
              status
              (list :log-byte-length (test-grid-utils::file-byte-length logfile)
                    :test-duration (/ (- (get-internal-real-time) start-time)
                                      internal-time-units-per-second))))))

(defun print-loadtest-log-header (system-name run-descr log-file)
  (with-open-file (stream (ensure-directories-exist log-file)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type 'character
                          :external-format tg-utils:*utf-8-external-format*)
    (let ((*print-case* :downcase) (*print-pretty* nil))
      (format stream "============================================================~%")
      (format stream "  cl-test-grid system load test~%")
      (format stream "------------------------------------------------------------~%")
      (format stream "  system:            ~A~%" system-name)
      (format stream "  lib-world:         ~A~%" (getf run-descr :lib-world))
      (format stream "  lisp:              ~A~%" (getf run-descr :lisp))
      (format stream "  contributor email: ~A~%" (getf run-descr :contact-email))
      (format stream "  timestamp:         ~A~%" (pretty-fmt-time (get-universal-time))))))

(defun proc-test-loading (lisp-exe system-name run-descr
                          logfile private-quicklisp-dir asdf-output-dir)
  (let ((start-time (get-internal-real-time))
        (status (handler-case
                    (ensure-list-response
                     (with-response-file (response-file)
                       (let* ((code `(progn
                                       (load ,(merge-pathnames "setup.lisp" private-quicklisp-dir))
                                       (load ,(src-file "proc-common.lisp"))
                                       (load ,(src-file "proc-common-asdf.lisp"))
                                       (load ,(src-file "proc-test-loading.lisp"))
                                       (cl-user::set-response
                                        ,response-file
                                        (cl-user::test-loading-main ,logfile
                                                                    ,system-name
                                                                    ,private-quicklisp-dir
                                                                    ,asdf-output-dir)))))
                         (log:info "Testing load of system ~A..." system-name)
                         (print-loadtest-log-header system-name run-descr logfile)
                         (lisp-exe:run-with-timeout +loadtest-timeout-seconds+ lisp-exe code))))
                  (no-response (condition)
                    (appending log logfile
                      (format log "~%Child lisp process loading the ~A system finished without returing result test status. "
                              system-name)
                      (format log "Looks like the lisp process has crashed. ")
                      (format log "The error condition signalled in the parent process: ~A" condition))
                    (log:info "Child lisp process seems crashed: didn't returned any response. The NO-RESPONSE error signalled: ~A"
                              condition)
                    '(:status :crash))
                  (lisp-exe:lisp-process-timeout (c)
                    (appending log logfile
                      (format log "~%The system ~A load hasn't finished in ~A seconds." system-name (lisp-exe:seconds c))
                      (format log "~%We consider the load operation as hung; the lisp process is killed.~%"))
                    (log:info "Child lisp testing loading the ~A system has exceeded the timeout of ~A seconds and killed."
                              system-name (lisp-exe:seconds c))
                    '(:status :timeout)))))
    (print-log-footer system-name (getf status :status) logfile)
    (log:info "The ~A system load result: ~S" system-name status)
    (append (list :system system-name)
            status
            (list :log-byte-length (test-grid-utils::file-byte-length logfile)
                  :load-duration (/ (- (get-internal-real-time) start-time)
                                    internal-time-units-per-second)))))

(defun loadtest-log-name (system-name)
  (format nil
          "~A-load"
          (substitute #\- #\.
                      ;; Substitute dots by hypens because CCL
                      ;; prepends the > symbol before dots (at least on windows);
                      ;; for example: hu.dwim.stefil => hu>.dwim.stefil.
                      ;; When we pass such a pathname to another lisps,
                      ;; they can't handle it.
                      (string-downcase system-name))))

(defun loadtest-log-file (test-run-directory system-name)
  (merge-pathnames (loadtest-log-name system-name)
                   (test-run-log-dir test-run-directory)))

(defun as-keyword (string-designator)
  (read-from-string (format nil ":~A" string-designator)))

(defun complete-test-run (test-run agent lisp-exe project-names)
  "Executes tests for the given LISP-EXE. The TEST-RUN may be
a fresh test run, or a test run partially done alredy (during
previous agent invocation). If the TEST-RUN directory already
contains results for some of the PROJECT-NAMES, these tests
are not repeated. Only the projects which don't have test
results in this directory are tested."
  (let* ((run-descr (test-grid-data::run-descr test-run))
         (run-dir (run-directory run-descr (test-output-base-dir agent))))
    (complete-test-run-impl test-run
                            run-dir
                            (private-quicklisp-dir agent)
                            lisp-exe
                            project-names
                            (alexandria:curry #'project-systems (project-lister agent)))))

(defun complete-test-run2 (description run-dir quicklisp-dir quicklisp-dist-name lisp-exe
                           &key project-names helper-lisp-exe)
  (unless (getf description :lib-world) (error "please specify :lib-world in the description"))
  (unless (getf description :contact-email) (error "lease specify :contact-email in the description"))
  (let* ((lisp-exe:*temp-dir* (or lisp-exe:*temp-dir* (ensure-directories-exist run-dir)))
         (run-info-file (run-info-file run-dir))
         (saved-test-run (when (probe-file run-info-file)
                           (test-grid-utils::safe-read-file run-info-file)))
         (test-run (make-run (tg-utils::merge-plists description
                                                     (list :lisp (implementation-identifier lisp-exe))
                                                     (test-grid-data::run-descr saved-test-run))
                             (test-grid-data::run-results saved-test-run)))
         (helper-lisp (or helper-lisp-exe lisp-exe))
         (project-lister (init-project-lister helper-lisp quicklisp-dir quicklisp-dist-name))
         (project-names (or project-names (project-names project-lister)))
         (project-systems-fn (alexandria:curry #'project-systems project-lister)))
    (save-run-info test-run run-dir)
    (complete-test-run-impl test-run run-dir quicklisp-dir lisp-exe project-names project-systems-fn)))

(defun complete-test-run-impl (test-run run-dir quicklisp-dir lisp-exe project-names project-systems-fn)
  (let* ((lisp-exe:*temp-dir* (or lisp-exe:*temp-dir* run-dir))
         (run-descr (test-grid-data::run-descr test-run))
         (asdf-output-dir (merge-pathnames "fasl/" run-dir))
         (lib-results (getf test-run :results))
         (start-time (get-universal-time))
         (prev-duration (if (numberp (getf run-descr :run-duration))
                            (getf run-descr :run-duration)
                            0)))
    (flet ((tested-p (project-name)
             (find (as-keyword project-name) lib-results :key (alexandria:rcurry #'getf :libname) :test #'eq))
           (check-asdf-output (subdir)
             (let ((asdf-output-subdir (merge-pathnames subdir asdf-output-dir)))
               (when (not (cl-fad:directory-exists-p asdf-output-subdir))
                 (log:warn "The ASDF output directroy ~S does not exist; seems like the test run was not using our asdf-output-translations and we have no guarantee all the sourcess were freshly recompiled." asdf-output-subdir)))))
      (let* ((total (length project-names))
             (project-names (remove-if #'tested-p project-names))
             (done (- total (length project-names))))
        (ensure-directories-exist run-dir)
        (dolist (project project-names)
          (log:info "Testing project ~A/~A ~A" (1+ done) total project)
          (let ((load-results '()))
            (dolist (system (funcall project-systems-fn project))
              (push (proc-test-loading lisp-exe system
                                       run-descr
                                       (loadtest-log-file run-dir system)
                                       quicklisp-dir
                                       asdf-output-dir)
                    load-results))
            (let* ((libname (find (as-keyword project) test-grid-testsuites:*all-libs*))
                   (lib-result (if libname
                                   (progn
                                     (log:info "Testsuite of project ~A" project)
                                     (proc-run-libtest lisp-exe libname run-descr
                                                       (lib-log-file run-dir project)
                                                       quicklisp-dir
                                                       asdf-output-dir))
                                   (list :libname (as-keyword project)))))
              (setf (getf lib-result :load-results) load-results)
              (push lib-result lib-results)

              ;; persist the currently computed results to disk
              (unless (getf run-descr :time)
                (setf (getf run-descr :time) start-time))
              (setf (getf run-descr :run-duration)
                    (+ prev-duration (- (get-universal-time) start-time)))
              (setf (getf test-run :descr) run-descr)
              (setf (getf test-run :results) lib-results)
              (save-run-info test-run run-dir)))
          (incf done))
        (log:info "The test results were saved to: ~%~A." (truename run-dir))
        (when project-names
          (check-asdf-output "ql/"))
        (when (intersection project-names test-grid-testsuites:*all-libs* :test #'string=)
          (check-asdf-output "test-grid/"))
        (ignore-errors ;; CCL on Windows has problem deleting directory if a file containing in it has total length of pathname longer than MAX_PATH - 1 = 259 chars.
          (cl-fad:delete-directory-and-files asdf-output-dir :if-does-not-exist :ignore))
        run-dir))))
