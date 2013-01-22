;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

(defun submit-logs (blobstore test-run-dir)
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
         (log-name-to-blobkey-alist (test-grid-gae-blobstore:submit-files2 blobstore
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

(defun submit-test-run-results (agent test-run-dir lisp-exe)
  (log:info "Submitting the test results to the server from the directory ~S ..." (truename test-run-dir))
  (let* ((run-info (submit-logs (blobstore agent) test-run-dir)))
    (log:info "The log files are submitted. Submitting the test run info...")
    (funcall (results-receiver agent) run-info lisp-exe)
    (log:info "Done. The test results are submitted.")
    run-info))

(defun submitted-p (test-run)
  "Tests whether the specified TEST-RUN is already
submitetd by checking if it contains a blobstore key
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
