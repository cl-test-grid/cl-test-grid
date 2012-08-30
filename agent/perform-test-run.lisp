
;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

(defun make-run-descr (lib-world lisp-implementation-identifier user-email)
  "Generate a description for a test run which might be
performed in the current lisp system."
  (list :lisp lisp-implementation-identifier
        :lib-world lib-world
        :time (get-universal-time)
        :run-duration :unknown
        :contact (list :email user-email)))

(defun fmt-time (universal-time &optional destination)
  "The preferred time format used in the cl-test-grid project."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time 0)
    (funcall #'format
             destination
             "~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
             year month date hour min sec)))

(defun name-run-directory (run-descr)
  "Generate name for the directory where test run
data (libraries test suites output and the run results) will be saved."
  (format nil
          "~A-~A"
          (fmt-time (getf run-descr :time))
          (getf run-descr :lisp)))

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
                         :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (test-grid-data::print-test-run out test-run))))

(defun lib-log-name (lib-name)
  (substitute #\- #\.
              ;; Substitute dots by hypens because CCL
              ;; prepends the > symbol before dots (at least on windows);
              ;; for example: hu.dwim.stefil => hu>.dwim.stefil.
              ;; When we pass such a pathname to another lisps,
              ;; they can't handle it.
              (string-downcase lib-name)))

(defun lib-log-file (test-run-directory lib-name)
  (merge-pathnames (lib-log-name lib-name) test-run-directory))

(defparameter +libtest-timeout-seconds+ #.(* 10 60)
  "Maximum number of seconds we give each library test suite
to complete. After this time we consider the test suite
as hung, kill the lisp process and record :TIMEOUT
as the library test result.")

(defparameter +loadtest-timeout-seconds+ #.(* 1 60)
  "Maximum number of seconds we give each ASDF system to load
in a fresh lisp process.  After this time we consider system
as hung, kill the lisp process and record :TIMEOUT
as the system load status.")

(defun pretty-fmt-time (universal-time &optional destination)
  "The human-readable time format, used in reports."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time 0)
    (funcall #'format
             destination
             "~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
             year month date hour min sec)))

(defmacro appending (stream-var file &body body)
  `(with-open-file (,stream-var ,file
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :append)
     ,@body))

(defun print-testsuite-log-header (libname run-descr log-file)
  (with-open-file (stream log-file
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (let ((*print-case* :downcase) (*print-pretty* nil))
      (format stream "============================================================~%")
      (format stream "  cl-test-grid testsuite execution~%")
      (format stream "------------------------------------------------------------~%")
      (format stream "  library:           ~A~%" libname)
      (format stream "  lib-world:         ~A~%" (getf run-descr :lib-world))
      (format stream "  lisp:              ~A~%" (getf run-descr :lisp))
      (format stream "  contributor email: ~A~%" (getf (getf run-descr :contact) :email))
      (format stream "  timestamp:         ~A~%" (pretty-fmt-time (get-universal-time)))
      (format stream "============================================================~%"))))

(defun print-log-footer (lib-or-system status log-file)
  (appending stream log-file
    (let ((*print-case* :downcase))
      (fresh-line stream)
      (terpri stream)
      (format stream "~%============================================================~%")
      (format stream "  cl-test-grid status for ~A: ~A~%"
              lib-or-system (test-grid-data::print-test-status nil status))
      (format stream "============================================================~%"))))

(defun proc-run-libtest (lisp-exe libname run-descr logfile private-quicklisp-dir asdf-output-dir)
  "Runs test-grid-testsuites::run-libtest in a separate process and returns the result."
  (let ((start-time (get-internal-real-time))
        (status (handler-case
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
                                                                                        ,asdf-output-dir)))))
                        (log:info "Starting ~A test suite..." libname)
                        (print-testsuite-log-header libname run-descr logfile)
                        (lisp-exe:run-with-timeout +libtest-timeout-seconds+ lisp-exe code)))
                  (no-response (condition)
                    (appending log logfile
                      (format log "~%Child lisp process running the ~A test suite finished without returing result test status."
                              libname)
                      (format log "Looks like the lisp process has crashed.")
                      (format log "The error condition signalled in the parent process: ~A" condition))
                    (log:info "Child lisp process seems crashed: didn't returned any response. The NO-RESPONSE error signalled: ~A"
                              condition)
                    :crash)
                  (lisp-exe:lisp-process-timeout (c)
                    (appending log logfile
                      (format log "~%The ~A test suite hasn't finished in ~A seconds." libname (lisp-exe:seconds c))
                      (format log "~%We consider the test suite as hung; the test suite lisp process is killed.~%"))
                    (log:info "Child lisp process running ~A test suite has exceeded the timeout of ~A seconds and killed."
                              libname (lisp-exe:seconds c))
                    :timeout))))
    (log:info "The ~A test suite status: ~S" libname status)
    (print-log-footer libname status logfile)
    (list :libname libname
          :status status
          :log-byte-length (test-grid-utils::file-byte-length logfile)
          :test-duration (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second))))

(defun perform-test-run (agent lib-world lisp-exe libs)
  (let* ((run-descr (make-run-descr lib-world
                                    (implementation-identifier lisp-exe)
                                    (user-email agent)))
         (run-dir (run-directory run-descr (test-output-base-dir agent)))
         (asdf-output-dir (merge-pathnames "asdf-output/" run-dir))
         (lib-results))
    (ensure-directories-exist run-dir)
    (dolist (lib libs)
      (let ((lib-result (proc-run-libtest lisp-exe lib run-descr
                                          (lib-log-file run-dir lib)
                                          (private-quicklisp-dir agent)
                                          asdf-output-dir)))
        (push lib-result lib-results)))
    (setf (getf run-descr :run-duration)
          (- (get-universal-time)
             (getf run-descr :time)))
    (let ((run (make-run run-descr lib-results)))
      (save-run-info run run-dir)
      (log:info "The test results were saved to: ~%~A." (truename run-dir))
      (dolist (asdf-output-subdir (list (merge-pathnames asdf-output-dir "private-quicklisp/")
                                        (merge-pathnames asdf-output-dir "test-grid/")))
        (when (not (cl-fad:directory-exists-p asdf-output-subdir))
          (log:warn "The ASDF output directroy ~S does not exist; seems like the test run was not using our asdf-output-translations and we have no guarantee all the sourcess were freshly recompiled." asdf-output-subdir)))
      run-dir)))

(defun submit-logs (blobstore test-run-dir)
  (let* ((run-info (test-grid-utils::safe-read-file (run-info-file test-run-dir)))
         ;; prepare parameters for the SUBMIT-FILES blobstore function
         (submit-params (mapcar #'(lambda (lib-result)
                                    (let ((libname (getf lib-result :libname)))
                                      (cons libname
                                            (lib-log-file test-run-dir libname))))
                                (test-grid-data::run-results run-info))))
    ;; submit files to the blobstore and receive
    ;; their blobkeys in response
    (let ((libname-to-blobkey-alist
           (test-grid-blobstore:submit-files blobstore
                                             submit-params)))
      ;; Now store the blobkeys for every library in the run-info.
      ;; Note, we destructively modify parts of the previously
      ;; read run-info.
      (flet ((get-blob-key (lib)
               (or (cdr (assoc lib libname-to-blobkey-alist))
                   (error "blobstore didn't returned blob key for the log of the ~A libary" lib))))
        (setf (test-grid-data::run-results run-info)
              (mapcar #'(lambda (lib-result)
                          (setf (getf lib-result :log-blob-key)
                                (get-blob-key (getf lib-result :libname)))
                          lib-result)
                      (test-grid-data::run-results run-info))))
      ;; finally, save the updated run-info with blobkeys
      ;; to the file. Returns the run-info.
      (save-run-info run-info test-run-dir)
      run-info)))

(defun submit-test-run-results (blobstore test-run-dir)
  (log:info "Submitting the test results to the server from the directory ~S ..." (truename test-run-dir))
  (let* ((run-info (submit-logs2 blobstore test-run-dir)))
    (log:info "The log files are submitted. Submitting the test run info...")
    (test-grid-blobstore:submit-run-info blobstore run-info)
    (log:info "Done. The test results are submitted. They will be reviewed by admin soon and added to the central database.")
    run-info))

;;; perform-test-run2

(defun print-loadtest-log-header (system-name run-descr log-file)
  (with-open-file (stream log-file
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (let ((*print-case* :downcase) (*print-pretty* nil))
      (format stream "============================================================~%")
      (format stream "  cl-test-grid system load test~%")
      (format stream "------------------------------------------------------------~%")
      (format stream "  system:            ~A~%" system-name)
      (format stream "  lib-world:         ~A~%" (getf run-descr :lib-world))
      (format stream "  lisp:              ~A~%" (getf run-descr :lisp))
      (format stream "  contributor email: ~A~%" (getf (getf run-descr :contact) :email))
      (format stream "  timestamp:         ~A~%" (pretty-fmt-time (get-universal-time)))
      (format stream "============================================================~%"))))

(defun proc-test-loading (lisp-exe system-name run-descr
                          logfile private-quicklisp-dir asdf-output-dir)
  (let ((start-time (get-internal-real-time))
        (status (handler-case
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
                        (log:info "Testing loading of system ~A..." system-name)
                        (print-loadtest-log-header system-name run-descr logfile)
                        (lisp-exe:run-with-timeout +loadtest-timeout-seconds+ lisp-exe code)))
                  (no-response (condition)
                    (appending log logfile
                      (format log "~%Child lisp process loading the ~A system finished without returing result test status."
                              system-name)
                      (format log "Looks like the lisp process has crashed.")
                      (format log "The error condition signalled in the parent process: ~A" condition))
                    (log:info "Child lisp process seems crashed: didn't returned any response. The NO-RESPONSE error signalled: ~A"
                              condition)
                    :crash)
                  (lisp-exe:lisp-process-timeout (c)
                    (appending log logfile
                      (format log "~%The system ~A load hasn't finished in ~A seconds." system-name (lisp-exe:seconds c))
                      (format log "~%We consider the load operation as hung; the lisp process is killed.~%"))
                    (log:info "Child lisp testing loading the ~A system has exceeded the timeout of ~A seconds and killed."
                              system-name (lisp-exe:seconds c))
                    :timeout))))
    (print-log-footer system-name status logfile)
    (log:info "The ~A system load status: ~S" system-name status)
    (list :system system-name
          :status status
          :log-byte-length (test-grid-utils::file-byte-length logfile)
          :load-duration (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second))))

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
  (merge-pathnames (loadtest-log-name system-name) test-run-directory))

(defun as-keyword (string-designator)
  (read-from-string (format nil ":~A" string-designator)))

(defun perform-test-run2 (agent lib-world lisp-exe project-names)
  (let* ((run-descr (make-run-descr lib-world
                                    (implementation-identifier lisp-exe)
                                    (user-email agent)))
         (run-dir (run-directory run-descr (test-output-base-dir agent)))
         (asdf-output-dir (merge-pathnames "asdf-output/" run-dir))
         (lib-results))
    (ensure-directories-exist run-dir)
    (dolist (project project-names)
      (log:info "Testing load of project ~A" project)
      (let ((load-results '()))
        (dolist (system (ql-dist:provided-systems (ql-dist:release project)))
          (push (proc-test-loading lisp-exe (ql-dist:name system)
                                   run-descr
                                   (loadtest-log-file run-dir (ql-dist:name system))
                                   (private-quicklisp-dir agent)
                                   asdf-output-dir)
                load-results))
        (let* (;; project may be a string, translate it to keyword first
               (libname (find project test-grid-testsuites:*all-libs* :test #'string-equal))
               (lib-result (if libname
                               (progn
                                 (log:info "Testsuite of project ~A" project)
                                 (proc-run-libtest lisp-exe libname run-descr
                                                   (lib-log-file run-dir project)
                                                   (private-quicklisp-dir agent)
                                                   asdf-output-dir))
                               (list :libname (as-keyword project)))))
          (setf (getf lib-result :load-results) load-results)
          (push lib-result lib-results))))
    (setf (getf run-descr :run-duration)
          (- (get-universal-time)
             (getf run-descr :time)))
    (let ((run (make-run run-descr lib-results)))
      (save-run-info run run-dir)
      (log:info "The test results were saved to: ~%~A." (truename run-dir))
      (dolist (asdf-output-subdir (list (merge-pathnames asdf-output-dir "private-quicklisp/")
                                        (merge-pathnames asdf-output-dir "test-grid/")))
        (when (not (cl-fad:directory-exists-p asdf-output-subdir))
          (log:warn "The ASDF output directroy ~S does not exist; seems like the test run was not using our asdf-output-translations and we have no guarantee all the sourcess were freshly recompiled." asdf-output-subdir)))
      run-dir)))

(defun submit-logs2 (blobstore test-run-dir)
  (let* ((run-info (test-grid-utils::safe-read-file (run-info-file test-run-dir)))
         ;; prepare parameters for the SUBMIT-FILES blobstore function:
         ;; alist ((<log file name> . <full log file path>) ...)
         (submit-params (mapcan (lambda (lib-result)
                                  (append 
                                   ;; the first element is the testsuite log of the project
                                   ;; (if the project has one)
                                   (when (getf lib-result :status)
                                     (let ((log-file (lib-log-file test-run-dir
                                                                   (getf lib-result :libname))))
                                       (list (cons (pathname-name log-file) log-file))))
                                   ;; rest are the loadtest logs for all the ASDF systems in that project
                                   (mapcar (lambda (load-result)
                                             (let ((log-file (loadtest-log-file test-run-dir
                                                                                (getf load-result :system))))
                                               (cons (pathname-name log-file) log-file)))
                                           (getf lib-result :load-results))))
                                (test-grid-data::run-results run-info)))
         (log-name-to-blobkey-alist (test-grid-blobstore:submit-files blobstore
                                                                      submit-params)))
    (flet ((get-blob-key (log-name)
                 (or (cdr (assoc log-name log-name-to-blobkey-alist :test #'string=))
                     (error "blobstore didn't returned blob key for the log file ~A" log-name))))
      ;; now patch the run-info with the received blob keys
      (setf (test-grid-data::run-results run-info)
            (mapcar (lambda (lib-result)
                      ;; set blobkey of the project testsuite (if the project has one)
                      (when (getf lib-result :status)
                        (setf (getf lib-result :log-blob-key)
                              (get-blob-key (lib-log-name (getf lib-result :libname)))))
                      ;; and blobkeys of loadtests for all the ASDF systems in the project
                      (setf (getf lib-result :load-results)
                            (mapcar (lambda (load-result)
                                      (setf (getf load-result :log-blob-key)
                                            (get-blob-key (loadtest-log-name (getf load-result :system))))
                                      load-result)
                                    (getf lib-result :load-results)))
                      lib-result)
                    (test-grid-data::run-results run-info))))
    ;; finally, save the updated run-info with blobkeys
    ;; to the file. Returns the run-info.
    (save-run-info run-info test-run-dir)    
    run-info))

(defun perform-test-run3 (test-run agent lisp-exe project-names)
  (let* ((run-descr (test-grid-data::run-descr test-run))
         (run-dir (run-directory run-descr (test-output-base-dir agent)))
         (asdf-output-dir (merge-pathnames "asdf-output/" run-dir))
         (lib-results (getf test-run :results))
         (start-time (get-universal-time))
         (prev-duration (if (numberp (getf run-descr :run-duration))
                            (getf run-descr :run-duration)
                            0)))
    (flet ((tested-p (project-name)
             (find (as-keyword project-name) lib-results :key (alexandria:rcurry #'getf :libname) :test #'eq)))
      (let ((project-names (remove-if #'tested-p project-names)))
        (ensure-directories-exist run-dir)
        (dolist (project project-names)
          (log:info "Testing load of project ~A" project)
          (let ((load-results '()))
            (dolist (system (ql-dist:provided-systems (ql-dist:release project)))
              (push (proc-test-loading lisp-exe (ql-dist:name system)
                                       run-descr
                                       (loadtest-log-file run-dir (ql-dist:name system))
                                       (private-quicklisp-dir agent)
                                       asdf-output-dir)
                    load-results))
            (let* ( ;; project may be a string, translate it to keyword first
                   (libname (find project test-grid-testsuites:*all-libs* :test #'string-equal))
                   (lib-result (if libname
                                   (progn
                                     (log:info "Testsuite of project ~A" project)
                                     (proc-run-libtest lisp-exe libname run-descr
                                                       (lib-log-file run-dir project)
                                                       (private-quicklisp-dir agent)
                                                       asdf-output-dir))
                                   (list :libname (as-keyword project)))))
              (setf (getf lib-result :load-results) load-results)
              (push lib-result lib-results)

              ;; persist the currently computed results to disk
              (setf (getf run-descr :run-duration)
                    (+ prev-duration (- (get-universal-time) start-time)))
              (setf (getf test-run :descr) run-descr)
              (setf (getf test-run :results) lib-results)
              (save-run-info test-run run-dir))))
        (log:info "The test results were saved to: ~%~A." (truename run-dir))
        (dolist (asdf-output-subdir (list (merge-pathnames asdf-output-dir "private-quicklisp/")
                                          (merge-pathnames asdf-output-dir "test-grid/")))
          (when (not (cl-fad:directory-exists-p asdf-output-subdir))
            (log:warn "The ASDF output directroy ~S does not exist; seems like the test run was not using our asdf-output-translations and we have no guarantee all the sourcess were freshly recompiled." asdf-output-subdir)))
        run-dir))))

(defun submitted-p (test-run)
  "Tests whether the specified TEST-RUN is already
submitetd by examining checking if it contains a blobstore key
for some log."
  (let ((lib-result (first (getf test-run :results))))
    (or (getf lib-result :log-blob-key)
        (let ((load-result (first (getf lib-result :load-results))))
          (getf load-result :log-blob-key)))))

(defun list-test-runs (test-runs-root-dir)
  (let ((test-runs '()))
    (dolist (child (cl-fad:list-directory test-runs-root-dir))
      (let ((run-info-file (merge-pathnames "test-run-info.lisp"
                                            child)))
        (when (probe-file run-info-file)
          (push (test-grid-utils::safe-read-file run-info-file) test-runs))))
    test-runs))

(defun list-unfinished-test-runs (test-runs-root-dir)
  (remove-if #'submitted-p (list-test-runs test-runs-root-dir)))


#|

(length (list-unfinished-test-runs #P"C:\\Users\\anton\\projects\\cl-test-grid2-work-dir2\\agent-dev\\test-runs\\"))

;; --- agent initialization --- 

(defparameter *ccl-1.8-x86* (make-instance 'lisp-exe:ccl
                                           :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl.exe"))

(defparameter *agent* (test-grid-agent:make-agent))

(setf (test-grid-agent:lisps *agent*) (list *ccl-1.8-x86*)
      (test-grid-agent:preferred-lisp *agent*) *ccl-1.8-x86*
      (test-grid-agent:user-email *agent*) "avodonosov@yandex.ru"
      (test-grid-agent:work-dir *agent*) #P"C:\\Users\\anton\\..\\anton\\projects\\cl-test-grid2-work-dir2\\agent-dev\\")

;(proc-test-loading cl-user::*ccl-1.8-x86-64* :zalexandria "alexandria-load" #P"/Users/anton/projects/cl-test-grid2-work-dir/agent/quicklisp/")


(update-testing-quicklisp *agent*)
(perform-test-run2 *agent*
                   "quicklisp 2012-08-11"
                   (preferred-lisp *agent*)
                   (subseq (mapcar #'ql-dist:name (ql-dist:provided-releases (ql-dist:dist "quicklisp")))
                           0 10))

(let ((run (first (list-unfinished-test-runs (test-output-base-dir *agent*)))))
  (perform-test-run3 run
                     *agent*
                     (preferred-lisp *agent*)
                     (subseq (mapcar #'ql-dist:name (ql-dist:provided-releases (ql-dist:dist "quicklisp")))
                             0 13)))

(time
 (let ((test-run-dir #P"C:\\Users\\anton\\projects\\cl-test-grid2-work-dir2\\agent-dev\\test-runs\\20120824192514-ccl-1.8-f95-win-x86\\"))
   (submit-logs2 (test-grid-gae-blobstore:make-blob-store :base-url "http://cl-test-grid.appspot.com")             
                 ;;(make-instance 'fake-blobstore)
                 test-run-dir)))


(lisp-features (preferred-lisp *agent*))

(ql-dist:provided-systems (ql-dist:release "time-interval"))

|#


#|

TODO:
- proc-test-loading:
  + P1 asdf-output-translations
  + P1 log header and footer
  + P1 remove loading test from run-libtest
  + P1 add log-length to load-result object
  - rename test-grid-testsuites:*all-libs* to *all-testsuites*?
  - should we test all the 366 ASDF systems provided by cl-glfw?
    The question is sent to their mailing list.
  - introduce generic function (test-grid-testsuites:systems <project-name>)
    with default implementation as (ql-dist:provided-systems (ql-dist:dist <project-name>))
- perform-test-run2:
  + P1 submit loading logs and store loading results in the test-run-info
  + P1 rename projects back to their original names (routes -> cl-routes)
  + pass the list of project names as parameter?
  - specify the dependency on quicklisp
- perform-test-run3:
  - clean asdf output directories before continuing a test-run?
- test-grid-data
  + printing for loadtest results
  - P1 db format: rename projects back to their original names (routes -> cl-routes)
  - db format: optimize, don't print testing status/duration/log-key when absent
    (relevant for new resuls where most of the systems don't have test suite)
    and don't pring load-results when absent (only releant for old results
    submitted before agent become able to test loading)
  - db format: the :LOAD-FAILED test status should be moved to :load-results collections
  - db format: rename
                  :libname -> :project
                  :status -> :test-status
                  :log-blob-key -> :test-log-blob-key
                  :log-byte-length -> :test-log-byte-length
  - project name: a keyword libname, or string?
+ unify calling proc-libtest and proc-test-loading
  + loading of proc-common.lisp
  + log header and footer code is contained in agent
  + remove duplicated version pretty-fmt-time from test-grid-testsuites module
  + saving-output, catching-problems
  + asdf-output-translations
- reporting
  - P1 handle the results where test status is NIL (i.e. the project has no testsuite)
  - P1 there will be no :LOAD-FAILED status?
    think more how to integrate load results with test results

|#
