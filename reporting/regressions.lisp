;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

;;; Alternative representation of a library test status.
;;;
;;; Fail-list is an always sorted list of failures.
;;;
;;; Failures [elements of this list] are strings - failed test
;;; names; or conses in the form (:unexpected-ok . "test-name")
;;; - names of the tests which passed successfully despite
;;; they are known to fail.
;;;
;;; The sort order is so that unexpected OKs are always
;;; at the end.
;;;
;;; Example:
;;; ("test-a2" "test-b" (:unexpected-ok "test-a1"))
;;;
;;; Note that fail-list does not distinguish known failures from unknwown failures;
;;; all of them are represented just by stings naming failed tests.

;; The sort order for failures
(defun fail-less (fail-a fail-b)
  (if (stringp fail-a)
      (if (stringp fail-b)
          (string< fail-a fail-b)  ; "test-a" "test-b"
          t)                       ; "test-a" (:unexpected-ok . "test-b")
      (if (stringp fail-b)
          nil                      ; (:unexpected-ok . "test-a") "test-b"
          (string< (cdr fail-a)    ; (:unexpected-ok . "test-a") (:unexpected-ok . "test-b")
                   (cdr fail-b)))))

;; Helper function for fail-list creation:
(defun sort-fail-list (fail-list)
  "Destructively sort the FAIL-LIST"
  (sort fail-list #'fail-less))

(assert (equal '("a" "a2" "a3" "b" "b2" "b3" (:unexpected-ok . "a2.2") (:unexpected-ok . "b2.2"))
               (sort-fail-list '("a" (:unexpected-ok . "a2.2") "b3" "b" "a2" "a3" (:unexpected-ok . "b2.2") "b2"))))

;; Convert the extended test status from the db.lisp form:
;;    (:failed-tests ("test-a" "test-b") :known-to-fail ("test-c"))
;; to the fail-list form:
;;    ("test-a" "test-b" (:unexpected-ok . "test-c"))
(defun fail-list (lib-status)
  (assert (listp lib-status))
  (let* ((failures (getf lib-status :failed-tests))
         (unexpected-oks (set-difference (getf lib-status :known-to-fail)
                                         failures
                                         :test #'string=)))
    (sort-fail-list (append failures
                            (mapcar #'(lambda (unexpected-ok-test) (cons :unexpected-ok unexpected-ok-test))
                                    unexpected-oks)))))
(assert
 (and (equal '() (fail-list '(:failed-tests ())))
      (equal '("a" "b") (fail-list '(:failed-tests ("a" "b"))))
      (equal '("a" "b") (fail-list '(:failed-tests ("b" "a"))))
      (equal '("a" "b" (:unexpected-ok . "c"))
             (fail-list '(:failed-tests ("a" "b") :known-to-fail ("b" "c"))))))

#|

In order to compare test statuses, we classify them into
the following type hierarchy.

  t  ---------------------- all statuses
    good  ----------------- all good statuses
      :ok
      extended-empty ------ extended status with empty :failed-tests and empty :known-to-fail
    :no-resouorce
    bad  ------------------ all bad statuses
      bad-symbol ---------- bad status represended by single keyword symbol
        :fail
        :crash
        :load-failed
        :timeoout
      extended-non-empty  - extended status with non-empty :failed-tests or :known-to-fail

As you can see, all the subtypes are disjoint.

The function OF-TYPE-P below implements the described type predicates.
|#

;; This table specifies how we comparte new test result
;; with old test result based on their types.
(defparameter *lib-status-regressions-rules*
  `( ;new-statys type     ;old-status type  ;does the new-status have regressions comparing to old-status?
    (good                 t                  nil)
    (:no-resource         t                  nil)
    (bad                  good               t)
    (bad                  :no-resource       nil)
    ;; coparisios between various bad results
    (extended-non-empty   bad-symbol         nil)
    (bad-symbol           extended-non-empty t)

    (:fail                bad-symbol         nil)
    (bad-symbol           bad-symbol         ,(lambda (new-status old-status)
                                                (not (eq new-status old-status))))

    (extended-non-empty   extended-non-empty ,(lambda (new-status old-status)
                                                (set-difference (fail-list new-status)
                                                                (fail-list old-status)
                                                                :test #'equal)))))
(defun of-type-p (lib-status lib-status-typespec)
  (ecase lib-status-typespec
    ((t) t)
    ((:ok :no-resource :fail :crash :load-failed :timeout)
     (eq lib-status lib-status-typespec))
    (bad-symbol (member lib-status
                        '(:fail :crash :load-failed :timeout)
                        :test #'eq))
    (extended-empty (and (listp lib-status)
                         (not (or (getf lib-status :failed-tests)
                                  (getf lib-status :known-to-fail)))))
    (extended-non-empty (and (listp lib-status)
                             (or (getf lib-status :failed-tests)
                                 (getf lib-status :known-to-fail))))
    (bad (or (of-type-p lib-status 'bad-symbol)
             (of-type-p lib-status 'extended-non-empty)))
    (good (or (of-type-p lib-status :ok)
              (of-type-p lib-status 'extended-empty)))))

(assert (of-type-p '(:failed-tests ("a")) 'extended-non-empty))
(assert (of-type-p '(:failed-tests ("a" "b") :known-to-fail ("a")) 'extended-non-empty))
(assert (of-type-p '() 'extended-empty))
(assert (not (of-type-p '(:failed-tests "a") 'extended-empty)))
(assert (of-type-p :fail :fail))
(assert (of-type-p :no-resource :no-resource))
(assert (of-type-p :ok :ok))
(assert (of-type-p :crash :crash))
(assert (of-type-p :load-failed :load-failed))
(assert (of-type-p :timeout :timeout))
(assert (of-type-p :crash 'bad-symbol))
(assert (of-type-p :timeout 'bad))
(assert (of-type-p :load-failed t))
(assert (of-type-p :ok t))
(assert (not (of-type-p '() 'extended-non-empty)))
(assert (of-type-p '() 'good))
(assert (of-type-p '(:failed-tests () :known-to-fail ("a" "b")) 'extended-non-empty))
(assert (of-type-p '(:failed-tests () :known-to-fail ("a" "b")) 'bad))
(assert (of-type-p '(:failed-tests () :known-to-fail ("a" "b")) t))

(defun has-regressions-p (new-lib-status old-lib-status)
  "Returns true if NEW-LIB-STATUS has regressions comparing to OLD-LIB-STATUS."
  (loop for (new-typespec old-typespec result-spec) in *lib-status-regressions-rules*
     do (when (and (of-type-p new-lib-status new-typespec)
                   (of-type-p old-lib-status old-typespec))
          (return-from has-regressions-p
            (if (typep result-spec 'function)
                (funcall result-spec new-lib-status old-lib-status)
                result-spec))))
  (error "Unrecognized lib-status combination. new-lib-status: ~S, old-lib-status: ~S"
         new-lib-status old-lib-status))

(assert (not (has-regressions-p :ok :fail)))
(assert (not (has-regressions-p :ok '(:failed-tests ("a")))))
(assert (not (has-regressions-p :no-resource :fail)))
(assert (not (has-regressions-p :no-resource :ok)))
(assert (not (has-regressions-p :no-resource :no-resource)))
(assert (not (has-regressions-p '(:failed-tests () :known-to-fail ()) :ok)))
(assert (not (has-regressions-p '(:failed-tests () :known-to-fail ()) :fail)))
(assert (not (has-regressions-p '(:failed-tests () :known-to-fail ()) '(:failed-tests ("a")))))
(assert (has-regressions-p '(:failed-tests ("a" "b")) '(:failed-tests ("c"))))
(assert (not (has-regressions-p '(:failed-tests ("a" "b")) '(:failed-tests ("a" "b")))))
(assert (has-regressions-p '(:failed-tests ("a" "b")) '()))
(assert (has-regressions-p '(:failed-tests ("a" "b")) :ok))
(assert (has-regressions-p '(:failed-tests () :known-to-fail ("a" "b"))
                           :ok))
(assert (has-regressions-p '(:failed-tests () :known-to-fail ("a" "b"))
                           '(:failed-tests () :known-to-fail ())))
(assert (has-regressions-p :fail :ok))
(assert (has-regressions-p :fail '(:failed-tests () :known-to-fail ())))
(assert (not (has-regressions-p '(:failed-tests () :known-to-fail ()) :fail)))
(assert (not (has-regressions-p :ok '(:failed-tests () :known-to-fail ()))))
(assert (not (has-regressions-p :fail :fail)))
(assert (has-regressions-p :crash :fail))
(assert (has-regressions-p :load-failed :fail))
(assert (has-regressions-p :timeout :fail))
(assert (has-regressions-p :timeout '(:failed-tests ("c"))))
(assert (has-regressions-p :timeout '()))
(assert (has-regressions-p :timeout :crash))
(assert (not (has-regressions-p :fail :crash)))

;; Diff item represent two results
;; of the same library under the same lisp,
;; but in different versions of quicklisp disto.
(defclass quicklisp-diff-item ()
  ((libname :initarg :libname :accessor libname)
   (lisp :initarg :lisp :accessor lisp)
   (new-result :initarg :new-result :accessor new-result)
   (old-result :initarg :old-result :accessor old-result)))

(defun new-status (quicklisp-diff-item)
  (status (new-result quicklisp-diff-item)))

(defun old-status (quicklisp-diff-item)
  (status (old-result quicklisp-diff-item)))

;; Diff of two quicklisp distro versions.
(defclass quicklisp-diff ()
  (;; diff-items where new quicklisp distro version has regressions
   ;; comparing to old version (note, at the same time
   ;; it may have improvements.
   ;;
   ;; For example previously test-a and test-b
   ;; failed, but in the new version test-a and test-c fail.
   ;; test-c is a regression - it fails now but not previously;
   ;; test-b is an improvement - it failed previously but not now;
   ;;
   ;; Independently on improvements presense, if the library
   ;; has regressiosn in new version, the quicklisp-diff-item
   ;; is put into the have-regressions list.
   (have-regressions :type list :initform '() :accessor have-regressions)
   ;; Here we put quicklisp-diff-itmes for libraries
   ;; which have only improvements in the new quicklisp distro
   ;; version.
   (imrovements-only :type list :initform '() :accessor improvements-only)))

(defun compare-quicklisps (db-index quicklisp-new quicklisp-old)
  "Returns QUICKLISP-DIFF for the two quicklisp distro versions
specified by QUICKLISP-NEW and QUICKLISP-OLD."
  (let* ((diff (make-instance 'quicklisp-diff))
         (lib-world-getter* (make-fields-values-getter '(:lib-world)))
         (lib-world-setter* (make-fields-values-setter '(:lib-world)))
         (lisp-getter* (make-fields-values-getter '(:lisp)))
         ;; todo: use alexandria:compose
         (lib-world-getter (lambda (index-key) (car (funcall lib-world-getter* index-key))))
         (lib-world-setter (lambda (index-key lib-world)
                             (funcall lib-world-setter* index-key (list lib-world))))
         (lisp-getter (lambda (index-key) (car (funcall lisp-getter* index-key))))
         (new-quicklisp-keys (remove quicklisp-new
                                     (test-grid-utils::hash-table-keys db-index)
                                     :key lib-world-getter
                                     :test (complement #'string=))))
    (dolist (key new-quicklisp-keys)
      (let ((key-prev (copy-list key)))
        (funcall lib-world-setter key-prev quicklisp-old)
        (let ((results (gethash key db-index))
              (results-prev (gethash key-prev db-index)))
          (dolist (joined-lib-result results)
            (let ((status (getf (lib-result joined-lib-result) :status)))
              (dolist (joined-lib-result-prev results-prev)
                (let ((status-prev (getf (lib-result joined-lib-result-prev) :status)))
                  (flet ((make-diff-item ()
                           (make-instance 'quicklisp-diff-item
                                          :libname (getf (lib-result joined-lib-result) :libname)
                                          :lisp (funcall lisp-getter key)
                                          :new-result joined-lib-result
                                          :old-result joined-lib-result-prev)))
                    (cond ((has-regressions-p status status-prev)
                           (push (make-diff-item)
                                 (have-regressions diff)))
                          ((has-regressions-p status-prev status)
                           (push (make-diff-item)
                                 (improvements-only diff))))))))))))
    ;; order our results-diff report by library name
    (setf (have-regressions diff)
          (sort (have-regressions diff) #'string< :key #'libname))
    (setf (improvements-only diff)
          (sort (improvements-only diff) #'string< :key #'libname))
    diff))

(defun print-quicklisp-diff (destination ql-new ql-old quicklisp-diff)
  (flet ((print-diff-item (diff-item)
           (let ((*print-pretty* nil))
             (format destination "~a, ~a:~%~a: <a class=\"~a\" href=\"~a\">~a</a>~%~a: <a class=\"~A\" href=\"~a\">~a</a>~%~%"
                     (string-downcase (libname diff-item))
                     (lisp diff-item)
                     ql-new
                     (status-css-class (aggregated-status (status (new-result diff-item))))
                     (lib-log-uri (new-result diff-item))
                     (status (new-result diff-item))
                     ql-old
                     (status-css-class (aggregated-status (status (old-result diff-item))))
                     (lib-log-uri (old-result diff-item))
                     (status (old-result diff-item))))))
    (format destination "~%~%***************************************************************************~%")
    (format destination "* test results diff between ~A and ~A *~%" ql-new ql-old)
    (format destination "***************************************************************************~%~%")
    (format destination "************* Have Regressions *************~%")
    (dolist (diff-item (have-regressions quicklisp-diff))
      (print-diff-item diff-item))
    (format destination "************* Improvements Only *************~%")
    (dolist (diff-item (improvements-only quicklisp-diff))
      (print-diff-item diff-item))))

(defun print-all-quicklisps-diff-report (destination joined-index)
  (format destination "<html><head>~%")
  (format destination "  <title>Quicklisps Diff - CL Test Grid</title>~%")
  (format destination "  <link href=\"style.css\" rel=\"stylesheet\"/><head>~%")
  (format destination "<head>~%")
  (format destination "<body><pre>~%")
  (let ((quicklisps (mapcar #'car (distinct-addresses joined-index '(:lib-world)))))
    (loop
       for qls on (sort quicklisps #'string>)
       do (let ((ql-new (first qls))
                (ql-old (second qls)))
            (when ql-old ;; not reached the end of list yet
              (print-quicklisp-diff destination
                                    ql-new
                                    ql-old
                                    (compare-quicklisps joined-index ql-new ql-old))))))
  (format destination "</pre></body>~%")
  (format destination "</html>~%"))
