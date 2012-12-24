;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; The test grid result storage named "main" is intended for storing
;;;; results of the last 3 quicklisps. When the test-grid-storage
;;;; is introduced, we start from populating it with test results
;;;; for the last 3 quicklisp releases from the db.lisp s-expression file
;;;; stored in test-grid-results git repository.
;;;;
;;;; The script below performs exactly this.
;;;;
;;;; The script was executed at 2012-12-14. I am not going
;;;; to maintain it afterwards.

(in-package #:cl-user)

;; (require #:test-grid-data)
;; (require #:test-grid-storage)

(defparameter *db* (test-grid-data::read-db))
(defparameter *r* (test-grid-storage:make-replica "main"
                                                  (asdf:system-relative-pathname :test-grid-storage
                                                                                 "db-main.lisp")))

(defun plist-equal (plist-1 plist-2)
  (and (= (length plist-1) (length plist-2))
       (alexandria:doplist (key val plist-1 t)
         (when (not (equal val (getf plist-2 key)))
           (return nil)))))

(assert (plist-equal '(:a 1 :b "2")
                     '(:a 1 :b "2")))

(assert (not (plist-equal '(:a 1 :b "2" :c '3)
                          '(:a 1 :b "2"))))

(assert (not (plist-equal '(:a 1)
                          '(:a 1 :b "2"))))

(defun test-run-equal (run-1 run-2)
  (plist-equal (test-grid-data::run-descr run-1)
               (test-grid-data::run-descr run-2)))

(defun add-test-run-if-new (replica test-run)
  (unless (find test-run (getf (sptm::data replica)
                               :runs)
                :test #'test-run-equal)    
    (sptm::repli-exec replica 'test-grid-data:add-test-run (list test-run))))

(defun test-runs-since-quicklisp (db min-quicklisp-prefix)
  (remove-if-not (lambda (test-run)
                   (string> (getf (test-grid-data::run-descr test-run)
                                  :lib-world)
                            min-quicklisp-prefix))
                 (getf db :runs)))

(defun copy-last-test-runs (db replica min-quicklisp-prefix)
  (let ((last-test-runs (test-runs-since-quicklisp db min-quicklisp-prefix)))
    (dolist (test-run (reverse last-test-runs))
      (format t "Adding ~S...~%" (test-grid-data::run-descr test-run))
      (add-test-run-if-new replica test-run)))
  (sptm::save-local-snapshot replica))


;; (copy-last-test-runs *db* *r* "quicklisp 2012-09")

