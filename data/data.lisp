;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(defpackage #:test-grid-data
  (:nicknames :tg-data)
  (:use :cl)
  (:export
   #:make-db
   #:join-dbs
   #:read-db
   #:save-db
   #:read-archive
   #:add-test-run
   #:add-test-runs
   #:remove-test-runs
   #:remove-lib-results
   #:update-run-descr))

(in-package #:test-grid-data)

(defun src-dir()
  (asdf:system-relative-pathname :test-grid-data #P"data/"))

(defun standard-archive-dir ()
  "The directory of cl-test-grid-results git repo, assuming it is cloned
near the cl-test-grid repo."
  (merge-pathnames #P"../../cl-test-grid-results/" (src-dir)))

;;; DB operations

(defun add-run (run-info db)
  "Deprecated. Modifies DB destructively."
  (push run-info (getf db :runs)))

(defun make-db (&optional test-runs)
  (list :schema 5 :runs test-runs))

(defun join-dbs (&rest dbs)
  (reduce (lambda (db1 db2)
            (make-db (append (getf db1 :runs)
                             (getf db2 :runs))))
          (cdr dbs)
          :initial-value (car dbs)))

(defun print-list-elements (destination list separator elem-printer)
  (let ((maybe-separator ""))
    (dolist (elem list)
      (format destination maybe-separator)
      (funcall elem-printer elem)
      (setf maybe-separator separator))))

(defun updated-plist (plist prop new-value)
  (let ((new (copy-list plist)))
    (setf (getf new prop) new-value)
    new))

(assert (= 2 (getf (updated-plist '(:a 1 :b 1) :a 2)
                   :a)))

(defun add-test-run (db test-run)
  ;; If DB is NIL, creates new DB automatically.
  ;; This is convenient because allows to execute
  ;; add-test-run transactions on test-grid-storage
  ;; without checking, whether DB has already been initialized.
  (make-db (cons test-run (getf db :runs))))

(defun add-test-runs (db test-runs)
  ;; If DB is NIL, creates new DB automatically.
  ;; This is convenient because allows to execute
  ;; add-test-run transactions on test-grid-storage
  ;; without checking, whether DB has already been initialized.
  (make-db (append test-runs (getf db :runs))))

(defun test-run-matcher (descr-key-val-plist)
  (let ((key-val-alist (alexandria:plist-alist descr-key-val-plist)))
    (lambda (test-run)
      (let ((descr (test-grid-data::run-descr test-run)))
        (every (lambda (key-val-cons)
                 (equal (getf descr (car key-val-cons))
                        (cdr key-val-cons)))
               key-val-alist)))))

(defun remove-test-runs (db &rest descr-key-val-plist)
  (updated-plist db :runs (remove-if (test-run-matcher descr-key-val-plist)
                                     (getf db :runs))))

(defun update-run-descr (db descr-key-val-plist new-key-vals)
  "DESCR-KEY-VAL-PLIST specifies what test run(s) to update.
NEW-KEY-VALS are new key-values for descriptions of that test runs."
  (let ((matcher (test-run-matcher descr-key-val-plist)))
    (updated-plist db
                   :runs
                   (mapcar (lambda (test-run)
                             (if (funcall matcher test-run)
                                 (updated-plist test-run
                                                :descr
                                                (tg-utils::merge-plists new-key-vals
                                                                        (run-descr test-run)))
                                 test-run))
                           (getf db :runs)))))

(defun remove-lib-results (db
                           test-run-descr-key-val-plist
                           libnames)
  (let* ((matcher (test-run-matcher test-run-descr-key-val-plist))
         (new-test-runs (mapcar (lambda (test-run)
                                  (if (funcall matcher test-run)
                                      (updated-plist test-run :results
                                                     (remove-if (lambda (lib-result)
                                                                  (member (getf lib-result :libname) libnames :test #'eq))
                                                                (getf test-run :results)))
                                      test-run))
                                (getf db :runs))))
    (updated-plist db :runs new-test-runs)))


;;; DB printing

(defun print-list (destination list separator elem-printer)
  (format destination "(")
  (print-list-elements destination list separator elem-printer)
  (format destination ")"))

(defun print-test-status (destination status)
  (etypecase status
    (symbol (format destination "~s" status))
    (list (progn
            (let ((dest (or destination (make-string-output-stream))))
              (flet ((test-name-printer (test-name)
                       (format dest "~s" test-name)))
                (format dest "(:failed-tests ")
                (print-list dest (sort (copy-list (getf status :failed-tests))
                                       #'string<)
                            " " #'test-name-printer)
                (format dest " :known-to-fail ")
                (print-list dest (sort (copy-list (getf status :known-to-fail))
                                              #'string<)
                            " " #'test-name-printer)
                (format dest ")"))
              (if (null destination)
                  (get-output-stream-string dest)
                  nil))))))

(defun run-descr (run)
  "The description part of the test run."
  (getf run :descr))

(defun run-results (run)
  "The list of test suite statuses for every library in the specified test run."
  (getf run :results))

(defun (setf run-results) (new-run-results test-run)
  (setf (getf test-run :results) new-run-results))

(defun print-test-run (out test-run &optional (indent 0))
  (let ((descr (getf test-run :descr)))
    (format out
            "(:descr (:lisp ~s :lib-world ~s :time ~s :run-duration ~s :contact-email ~s)~%"
            (getf descr :lisp)
            (getf descr :lib-world)
            (getf descr :time)
            (getf descr :run-duration)
            (getf descr :contact-email)))
  (format out "~v,0t:results (" (1+ indent))
  (print-list-elements out
                       (sort (copy-list (getf test-run :results))
                             #'string<
                             :key #'(lambda (lib-result)
                                      (getf lib-result :libname)))
                       (format nil "~~%~~~Dt" (+ indent 11))
                       #'(lambda (lib-result)
                           (format out
                                   "(:libname ~s"
                                   (getf lib-result :libname))
                           (when (getf lib-result :status)
                             (format out
                                     " :status ~a :log-blob-key ~s :log-byte-length ~s :test-duration ~s"
                                     (print-test-status nil (getf lib-result :status))
                                     (getf lib-result :log-blob-key)
                                     (getf lib-result :log-byte-length)
                                     (getf lib-result :test-duration))
                             (when (getf lib-result :fail-condition-type)
                               (format out
                                       " :fail-condition-type ~s :fail-condition-text ~s"
                                       (getf lib-result :fail-condition-type)
                                       (getf lib-result :fail-condition-text))))
                           (when (getf lib-result :load-results)
                             (format out "~%~v,0t:load-results (" (+ indent 12))
                             (print-list-elements out
                                                  (sort (copy-list (getf lib-result :load-results))
                                                        #'string<
                                                        :key #'(lambda (load-result)
                                                                 (getf load-result :system)))
                                                  (format nil "~~%~~~Dt" (+ indent 27))
                                                  (lambda (load-result)
                                                    (format out "(:system ~s :status ~s :log-blob-key ~s :log-byte-length ~s :load-duration ~s"
                                                            (getf load-result :system)
                                                            (getf load-result :status)
                                                            (getf load-result :log-blob-key)
                                                            (getf load-result :log-byte-length)
                                                            (getf load-result :load-duration))
                                                    (when (getf load-result :fail-condition-type)
                                                      (format out
                                                              " :fail-condition-type ~s :fail-condition-text ~s"
                                                              (getf load-result :fail-condition-type)
                                                              (getf load-result :fail-condition-text)))
                                                    (format out ")")))
                             (format out ")"))
                           (format out ")")))
  (format out "))"))

(defun print-db (out db &optional (indent 0))
  (format out "(:schema ~a~%" (getf db :schema))
  (format out "~v,0t :runs (" indent)
  (print-list-elements out
                       (getf db :runs)
                       (format nil "~%~v,0t" (+ indent 8))
                       #'(lambda (test-run)
                           (print-test-run out test-run (+ indent 8))))
  (format out "))"))

(defun save-db (db stream-or-path)
  (with-open-file (out stream-or-path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type 'character
                       :external-format tg-utils:*utf-8-external-format*)
    (print-db out db)))

(defun read-db (stream-or-path)
  (with-open-file (in stream-or-path
                      :direction :input
                      :element-type 'character
                      :external-format tg-utils:*utf-8-external-format*)
    (test-grid-utils::safe-read in)))

(defun archive-files (&optional (archive-dir (standard-archive-dir)))
  ;; All the existing files named from db01.lisp to db99.lisp
  ;; inside the archive-dir, ordered by names.
  ;; Stops at the first non-existing file, so for example,
  ;; if the directory contains db01.lisp, db02.lisp and db05.lisp,
  ;; only db01.lisp and db02.lisp are listed.
  (flet ((archive-file (file-name)
           (merge-pathnames file-name archive-dir)))
    (let ((result nil))
      (loop for i from 1 below 100
         do (let* ((cur-file-name (format nil "db~2,'0d.lisp" i))
                   (cur-file (archive-file cur-file-name)))
              (when (not (probe-file cur-file))
                (return-from archive-files (nreverse result)))
              (setq result (cons cur-file result)))))))

(defun read-archive (&key (archive-dir (standard-archive-dir))
                       last-n)
  (let ((files (archive-files archive-dir)))
    (apply #'join-dbs
           (mapcar #'read-db
                   (if last-n
                       (last files last-n)
                       files)))))

