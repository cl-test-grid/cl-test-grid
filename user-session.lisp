;;;; User (my) session

;; In my system CLISP doesn't execute .clisprc when started
;; from SLIME, therefore quicklisp is not available
#+clisp 
(load "/Users/anton/.clisprc")

(pushnew "D:/cl-test-grid/" asdf:*central-registry* :test #'equal)

(pushnew "C:/Users/anton/projects/cl-test-grid/" asdf:*central-registry* :test #'equal)

(asdf:operate 'asdf:load-op :test-grid)

;; for development of GAE blob storage
;;(setf test-grid::*gae-blobstore-base-url* "http://localhost:8080")

(use-package :ql-dist)
(available-versions (dist "quicklisp"))

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

(setf *db* '(:version 0 :runs ()))
(setf *db* (read-db))


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

(generate-reports *db*)

(let ((ql-new "quicklisp 2012-02-08")
      (ql-old "quicklisp 2012-01-07")
      (idx (build-joined-index *db*)))
  (print-quicklisp-diff ql-new
                        ql-old
                        (compare-quicklisps idx ql-new ql-old)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; developer experiments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sort test runs in the database according to some criteria
(setf (getf *db* :runs)
      (sort (getf *db* :runs)
            (plist-comparator :lisp #'string<)
            :key #'first))


;; generate fake database content to test reporting
(setf (getf *db* :runs) (generate-fake-run-results))
    
(print-pivot-reports *db*)

