;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: test-grid; Base: 10; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

;;;; User (my) session

;; In my system CLISP doesn't execute .clisprc when started
;; from SLIME, therefore quicklisp is not available
#+clisp
(load "/Users/anton/.clisprc")

(pushnew "D:/cl-test-grid/" asdf:*central-registry* :test #'equal)

(pushnew "C:/Users/anton/projects/cl-test-grid/" asdf:*central-registry* :test #'equal)

(asdf:operate 'asdf:load-op :test-grid-reporting)
(asdf:operate 'asdf:load-op :test-grid-tests)

;; for development of GAE blob storage
;;(setf test-grid::*gae-blobstore-base-url* "http://localhost:8080")

(use-package :ql-dist)
(available-versions (dist "quicklisp"))
(ql:update-dist "quicklisp")

;;(install-dist "http://beta.quicklisp.org/dist/quicklisp/2011-08-29/distinfo.txt" :replace t)
;;(ql:quickload :quicklisp-slime-helper)

(in-package #:test-grid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; user tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf run-result
      (run-libtests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; admin tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op :test-grid-admin)
(test-grid-admin:import-test-result-emails "some secret password")
;; Now review git diff db.lisp.
;;
;; Generate and review reports if necessary before commit:
(test-grid-reporting::generate-reports (test-grid::read-db))
;;
;; Commit db.lisp with the commit message printed
;; by IMPROT-TEST-RESULT-EMAILS to the standrard output.
;;
;; After than we can delete the emails:
(test-grid-admin:delete-imported-emails "some secret password")

;;; Old code for manual import of test results
(setf *db* '(:version 0 :runs ()))
(progn
  (setf *db* (read-db))
  nil)


(progn
  (add-run (safe-read-file "mail.lisp")
           *db*)
  nil)

(progn
  (setf run-result
        (submit-results #P"C:\\Users\\anton\\projects\\cl-test-grid\\test-runs\\20120214013817-abcl-1.0.1-svn-13750-13751-fasl38-solaris-x86\\"))
  nil)

(setf run-result
      (safe-read-file #P"C:\\Users\\anton\\projects\\cl-test-grid\\test-runs\\20120214011952-sbcl-1.0.54.84.mswinmt.1137-215bdc8-win-x64\\test-run-info.lisp"))

(progn
  (add-run run-result *db*)
  nil)

(save-db *db*)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; developer experiments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; print list of CFFI failed tests, and for every failure
;; specify the Lisp implementations where it occurs.
(let ((cffi-failures (make-hash-table :test #'equal)))
  (test-grid-reporting::do-results (test-run lib-result test-grid::*db*)
    (when (and (string= "quicklisp 2012-02-08" (getf (getf test-run :descr) :lib-world))
               (eq :cffi (getf lib-result :libname)))
      (let ((status (getf lib-result :status)))
        (when (consp status)
          (dolist (failure (getf status :failed-tests))
            (pushnew (getf (getf test-run :descr) :lisp)
                     (gethash failure cffi-failures)
                     :test #'string=))))))

  (dolist (failure (sort (test-grid::hash-table-keys cffi-failures) #'string<))
    (format t "~S => ~S~%" failure (sort (gethash failure cffi-failures) #'string<))))

;; sort test runs in the database according to some criteria
(setf (getf *db* :runs)
      (sort (getf *db* :runs)
            (plist-comparator :lisp #'string<)
            :key #'first))


;; generate fake database content to test reporting
(setf (getf *db* :runs) (generate-fake-run-results))

(print-pivot-reports *db*)
