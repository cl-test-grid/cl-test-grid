;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun list-failures (lib-results)
  (mapcan #'failures lib-results))

(defun failures (lib-result)
  (nconc (test-failures lib-result)
         (load-failures lib-result)))

(defun test-failures (lib-result)
  (let* ((test-status (status lib-result))
         (test-fail-specs (etypecase test-status
                            (keyword (when (and (not (eq :ok test-status))
                                                (not (eq :no-resource test-status)))
                                       (list (list :whole-test-suite test-status))))
                            (list (let* ((failures (getf test-status :failed-tests))
                                         (unexpected-oks (set-difference (getf test-status :known-to-fail)
                                                                         failures
                                                                         :test #'string=)))
                                    (nconc (mapcar (lambda (failed-test-case) (list :test-case failed-test-case :fail))
                                                   failures)
                                           (mapcar (lambda (unexpected-ok-test) (list :test-case unexpected-ok-test :unexpected-ok))
                                                   unexpected-oks)))))))
    (mapcar (lambda (fail-spec)
              (make-instance 'failure :lib-result lib-result :fail-spec fail-spec))
            test-fail-specs)))

(defun load-failures (lib-result)
  (mapcar (lambda (load-result)
            (let ((fail-spec (list :load (system-name load-result) (load-status load-result))))
              (make-instance 'failure :lib-result lib-result :fail-spec fail-spec :load-result load-result)))
          (remove-if (lambda (load-status)
                       (or (eq :ok load-status) (eq :no-resource load-status)))
                     (load-results lib-result)
                     :key #'load-status)))

(defclass failure ()
  ((lib-result :type joined-lib-result :initarg :lib-result :reader lib-result)
   (load-result :type (or nul list) :initarg :load-result :initform nil :reader load-result)
   (fail-spec :type list :initarg :fail-spec :reader fail-spec)))

(defmethod lisp ((item failure))
  (lisp (lib-result item)))
(defmethod lib-world ((item failure))
  (lib-world (lib-result item)))
(defmethod libname ((item failure))
  (libname (lib-result item)))
(defmethod log-blob-key ((item failure))
  (if (load-result item)
      (log-blob-key (load-result item))
      (log-blob-key (lib-result item))))
(defmethod log-byte-length ((item failure))
  (if (load-result item)
      (log-byte-length (load-result item))
      (log-byte-length (lib-result item))))
(defmethod contact-email ((item failure))
  (contact-email (lib-result item)))

(defparameter *db* (test-grid-data:read-db))
(defparameter *pr* (select *db*))
(defparameter *fails* (list-failures *pr*))

(let* (
       ;;(last-abcl (first (largest #'lisp *db* :where (lambda (lib-result)
       ;;                                                (search "abcl" (lisp lib-result))))))
       (last-quicklisp "quicklisp 2012-08-11")
       (last-abcl "abcl-1.1.0-dev-svn-14149-fasl39-linux-java")
       (last-abcl-fails (remove-if-not (lambda (failure)
                                         (and
                                          (string= (lisp failure) last-abcl)
                                          (string= (lib-world failure) last-quicklisp)))
                                       *fails*))
       (abcl-1.0.1-fails (remove-if-not (lambda (failure)
                                          (and (string= (lisp failure) *abcl-1.0.1-impl*)
                                               (string= (lib-world failure) last-quicklisp)))
                                        *fails*))
       (diff (set-exclusive-or last-abcl-fails
                               abcl-1.0.1-fails
                               :test #'equal
                               :key (lambda (failure)
                                      (list (fail-spec failure)
                                            (libname failure))))))
  (with-report-file (out "failures.html")
    (pivot-report out (cl:with-output-to-string (str)
                        (pivot-table-html2 str diff
                                           (list #'libname) (list #'string<)
                                           (list #'lib-world #'lisp) (list #'string> #'string<)
                                           (lambda (out cell-data) (format out "~{~S<br/>~}" (mapcar #'fail-spec cell-data))))))))


(with-report-file (out "failures.lisp")
  (dolist (fail *fails*)
    (format out "~S ~S ~S ~S~%" (libname fail) (fail-spec fail) (lisp fail) (lib-world fail) )))


(let* (
       ;;(last-abcl (first (largest #'lisp *db* :where (lambda (lib-result)
       ;;                                                (search "abcl" (lisp lib-result))))))
       (last-quicklisp "quicklisp 2012-08-11")
       (last-abcl "abcl-1.1.0-dev-svn-14149-fasl39-linux-java")
       (last-abcl-fails (time (progn (format t "remove-if-not~%")
                                     (remove-if-not (lambda (failure)
                                                      (and
                                                       (string= (lisp failure) last-abcl)
                                                       (string= (lib-world failure) last-quicklisp)))
                                                    *fails*))))
       (abcl-1.0.1-fails (time (progn (format t "remove-if-not~%")
                                      (remove-if-not (lambda (failure)
                                                       (and (string= (lisp failure) *abcl-1.0.1-impl*)
                                                            (string= (lib-world failure) last-quicklisp)))
                                                     *fails*))))
       (diff (time (progn (format t "set-exclisive-or~%")
                          (set-exclusive-or last-abcl-fails
                                            abcl-1.0.1-fails
                                            :test #'equal
                                            :key (lambda (failure)
                                                   (list (fail-spec failure)
                                                         (libname failure))))))))
  (with-report-file (out "failures.html")
    (pivot-report out (cl:with-output-to-string (str)
                        (pivot-table-html2 str diff
                                           (list #'libname) (list #'string<)
                                           (list #'lib-world #'lisp) (list #'string> #'string<)
                                           (lambda (out cell-data)
                                             (dolist (fail cell-data)
                                               (format out "~A</br>" (failure-log-link fail #'fail-spec)))))))))


