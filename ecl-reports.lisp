;;;; This file generates reports for ECL.
;;;;
;;;; It accompanies the discussion on the cl-test-grid group:
;;;; https://groups.google.com/forum/?fromgroups=#!topic/cl-test-grid/4Or2QNPYeRQ
;;;;
;;;; The code expects Quicklisp is available.

(format t "*** loading test-grid-reporting ASDF system...~%")

(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))
  (pushnew this-file-dir asdf:*central-registry* :test #'equal))

(ql:quickload :test-grid-reporting)
(ql:quickload :local-result-store)

;; The test-grid-reporting package does 
;; not export any public API yet,
;; because the API is only being formed out.
;;
;; Let's enter the package to access
;; it's functions without typing package prefix.
(in-package :test-grid-reporting)

(format t "*** reading the test results database...~%")

(defparameter *local-db-path* (truename (ensure-directories-exist #p"ecl-reports/local-results/")))

(defparameter *db*
  (list :version 4
        :runs (local-result-store:list-test-run-results *local-db-path*)
  ;; alternatively, if you want to use the oficcial resutls DB,
  ;; first git clone git@github.com:cl-test-grid/cl-test-grid-results.git
  ;; and use:
  ;; (test-grid-data:read-db)
  )

(defparameter *all-results* (list-results *db*))

(defparameter *last-quicklisp* (first (largest #'lib-world *all-results* :predicate #'string>)))

(defparameter *last-ecl-version* "bca1f405")

(format t "*** overview of all the results of the last ECL on the lates Quicklisp...~%")
;;; This table will include all the ECL kinds
;;; (bytecode and lisp-to-c, linux, windows, etc.)
;;; Every ECL will have a separate column.
(print-pivot "ecl/ecl-results.html"
             (subset *all-results*
                     (lambda (result)
                       (and (string= *last-quicklisp* (lib-world result))
                            (search "ecl" (lisp result))
                            (search *last-ecl-version* (lisp result)))))
             :rows '((libname string<))
             :cols '((lib-world string>) (lisp string<))
             :cell-printer #'results-cell-printer)

(let ((new-ecl-lisp-to-c "ecl-12.7.1-bca1f405-linux-x86-lisp-to-c")
      (old-ecl-lisp-to-c "ecl-12.7.1-ce653d88-linux-x86-lisp-to-c")
      (new-ecl-bytecode "ecl-12.7.1-bca1f405-linux-x86-bytecode")
      (old-ecl-bytecode "ecl-12.7.1-ce653d88-linux-x86-bytecode"))

  (format t "*** diff between new and old ECL...~%")

  (print-compiler-diff "ecl/ecl-diff-lisp-to-c.html"
                       *all-results*
                       *last-quicklisp*
                       old-ecl-lisp-to-c
                       new-ecl-lisp-to-c)
  
  (print-compiler-diff "ecl/ecl-diff-bytecode.html"
                       *all-results*
                       *last-quicklisp*
                       old-ecl-bytecode
                       new-ecl-bytecode)
  
  (format t "*** load failures + dependencies...~%")
  
  (print-load-failures "ecl/ecl-load-failures-bytecode.html"
                       *all-results*
                       "ecl-12.7.1-bca1f405-linux-x86-bytecode"
                       *last-quicklisp*)
  
  (print-load-failures "ecl/ecl-load-failures-lisp-to-c.html"
                       *all-results*
                       "ecl-12.7.1-bca1f405-linux-x86-lisp-to-c"
                       *last-quicklisp*))

(format t "*** ECL reports are placed into cl-test-gird/reports-generated/ecl/~%")
(format t "*** Bye!~%")
