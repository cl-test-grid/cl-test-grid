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

(defun lib-log-file (test-run-directory lib-name)
  (merge-pathnames (substitute #\- #\.
                               ;; Substitute dots by hypens because CCL
                               ;; prepends the > symbol before dots (at least on windows);
                               ;; for example: hu.dwim.stefil => hu>.dwim.stefil.
                               ;; When we pass such a pathname to another lisps,
                               ;; they can't handle it.
                               (string-downcase lib-name))
                   test-run-directory))

(defparameter +libtest-timeout-seconds+ #.(* 30 60) ;; half an hour
  "Maximum number of seconds we give each library test suite
to complete. After this time we consider the test suite
as hung, kill the lisp process and record a :FAIL
as the library test result.")

(defun proc-run-libtest (lisp-exe libname run-descr logfile private-quicklisp-dir asdf-output-dir)
  "Runs test-grid-testsuites::run-libtest in a separate process and returns the result."
  (flet ((finish-test-log-with-failure (format-control &rest format-arguments)
           ;; Helper function to record failure status to the library
           ;; test log if the child lisp process running
           ;; the test suite has terminated abnormally.
           ;; (In case of successful termination it is
           ;; responsibility of the child process to print
           ;; the log footer. We already have a TODO item to move
           ;; all the log header/footer printing to the
           ;; parent process.)
           (with-open-file (out logfile
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :append)
             (apply #'format out format-control format-arguments)
             (test-grid-testsuites::print-log-footer libname :fail out))))
    (let ((start-time (get-internal-real-time))
          (status (handler-case
                      (with-response-file (response-file)
                        (let* ((code `(progn
                                        (load ,(merge-pathnames "setup.lisp" private-quicklisp-dir))
                                        (load ,(src-file "proc-run-libtest.lisp"))
                                        (cl-user::run-libtest-with-response-to-file ,libname
                                                                                    (quote ,run-descr)
                                                                                    ,logfile
                                                                                    ,private-quicklisp-dir
                                                                                    ,asdf-output-dir
                                                                                    ,response-file))))
                          (log:info "preparing to start separate lisp process with code: ~S" code)
                          (lisp-exe:run-with-timeout +libtest-timeout-seconds+ lisp-exe code)))
                    (no-response (condition)
                      (log:info "Child lisp process seems crashed: didn't returned any response. The NO-RESPONSE error signalled: ~A"
                                condition)
                      (finish-test-log-with-failure "~%Child lisp process running the ~A test suite finished without returing result test status. Looks like the lisp process has crashed. The error condition signalled: ~A"
                                                    libname condition)
                      :crash)
                    (lisp-exe:lisp-process-timeout ()
                      (log:info "Child lisp running the ~A test suite has exceeded the timeout of ~A seconds and killed."
                                libname +libtest-timeout-seconds+)
                      (finish-test-log-with-failure "~%The ~A test suite hasn't finished in ~A seconds.~%We consider the test suite as hung; the test suite lisp process is killed.~%"
                                                    libname +libtest-timeout-seconds+)
                      :timeout))))
      (log:info "The ~A test suite status: ~S" libname status)
      (list :libname libname
            :status status
            :log-byte-length (test-grid-utils::file-byte-length logfile)
            :test-duration (/ (- (get-internal-real-time) start-time)
                              internal-time-units-per-second)))))

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
  (let* ((run-info (submit-logs blobstore test-run-dir)))
    (log:info "The log files are submitted. Submitting the test run info...")
    (test-grid-blobstore:submit-run-info blobstore run-info)
    (log:info "Done. The test results are submitted. They will be reviewed by admin soon and added to the central database.")
    run-info))
