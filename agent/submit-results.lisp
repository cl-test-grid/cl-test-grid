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
                                   (when (and (getf lib-result :status)
                                              (not (getf lib-result :log-blob-key)))
                                     (let ((log-file (lib-log-file test-run-dir
                                                                   (getf lib-result :libname))))
                                       (list (cons (pathname-name log-file) log-file))))
                                   ;; rest are the loadtest logs for all the ASDF systems in that project
                                   (mapcar (lambda (load-result)
                                             (let ((log-file (loadtest-log-file test-run-dir
                                                                                (getf load-result :system))))
                                               (cons (pathname-name log-file) log-file)))
                                           (remove-if (lambda (load-result)
                                                        (getf load-result :log-blob-key))
                                                      (getf lib-result :load-results)))))
                                (test-grid-data::run-results run-info)))
         (log-name-to-blobkey-alist (test-grid-gae-blobstore:submit-files2 blobstore
                                                                           submit-params)))
    (flet ((get-blob-key (log-name)
             (cdr (assoc log-name log-name-to-blobkey-alist :test #'string=))))
      (dolist (submit-param submit-params)
        (unless (get-blob-key (car submit-param))
          (error "blobstore hasn't returned blob key for the log file ~A" (car submit-param))))
      ;; now patch the run-info with the received blob keys
      (setf (test-grid-data::run-results run-info)
            (mapcar (lambda (lib-result)
                      ;; set blobkey of the project testsuite (if the project has one)
                      (when (getf lib-result :status)
                        (setf (getf lib-result :log-blob-key)
                              (or (get-blob-key (lib-log-name (getf lib-result :libname)))
                                  (getf lib-result :log-blob-key))))
                      ;; and blobkeys of loadtests for all the ASDF systems in the project
                      (setf (getf lib-result :load-results)
                            (mapcar (lambda (load-result)
                                      (setf (getf load-result :log-blob-key)
                                            (or (get-blob-key (loadtest-log-name (getf load-result :system)))
                                                (getf load-result :log-blob-key)))
                                      load-result)
                                    (getf lib-result :load-results)))
                      lib-result)
                    (test-grid-data::run-results run-info))))
    ;; finally, save the updated run-info with blobkeys
    ;; to the file. Returns the run-info.
    (save-run-info run-info test-run-dir)
    run-info))

(defun submitted-p (test-run)
  "Tests whether the specified TEST-RUN is already
submitetd by checking if it contains a blobstore key
for some log."
  (let ((lib-result (first (getf test-run :results))))
    (or (getf lib-result :log-blob-key)
        (let ((load-result (first (getf lib-result :load-results))))
          (getf load-result :log-blob-key)))))

(defun make-gae-blobstore ()
  (test-grid-gae-blobstore:make-blob-store :base-url
                                           ;; during development of GAE blob storage
                                           ;; :base-url may be "http://localhost:8080"
                                           "http://cl-test-grid.appspot.com"))

(defun submit-test-run-results (test-run-dir storage-names)
  (log:info "Submitting the test results to the server from the directory ~S ..." (truename test-run-dir))
  (let* ((blobstore (make-gae-blobstore))
         (run-info (submit-logs blobstore test-run-dir)))
    (log:info "The log files are submitted. Submitting the test run info...")
    (tg-utils::write-to-file (mapcar (lambda (storage)
                                       (cons storage
                                             (test-grid-storage:add-test-run storage
                                                                             run-info)))
                                     storage-names)
                             (merge-pathnames "committed-versions.lisp"
                                              test-run-dir))
    (send-notification (format nil "[test run submitted] [storages: ~{~A~^, ~}]"
                               storage-names)
                       (format nil "~S" (tg-data::run-descr run-info)))
    (log:info "Done. The test results are submitted.")
    run-info))

(defun list-unfinished-test-runs (test-runs-root-dir)
  (let ((test-runs '()))
    (dolist (child (cl-fad:list-directory test-runs-root-dir))
      (let ((run-info-file (merge-pathnames "test-run-info.lisp"
                                            child)))
        (when (and (probe-file run-info-file)
                   (not (probe-file (merge-pathnames "comitted-versions.lisp"
                                                     child))))
          (push (tg-utils::safe-read-file run-info-file) test-runs))))
    test-runs))
