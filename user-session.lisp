;; In my system CLISP doesn't execute .clisprc when started
;; from SLIME, therefore quicklisp is not available
#+clisp 
(load "/Users/anton/.clisprc")
;;(load "D:\Program Files\Steel Bank Common Lisp\1.0.53.19.mswinmt.1042-de54330
(pushnew "D:/cl-test-grid/"
         asdf:*central-registry*)
(user-homedir-pathname)
;;(load "D:\\Documents and Settings\\Administrator\\quicklisp\\setup.lisp")
;;(ql-impl-util:add-to-init-file)
(asdf:operate 'asdf:load-op :test-grid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User (my) session
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:test-grid)

(setf *db* '(:version 0 :runs ()))

(setf *db* (read-db))
(setf run-result
      (run-libtests))
(add-run run-result)
(save-db)

(defun read-file (file)
  (with-open-file (in file
                      :direction :input
                      :if-does-not-exist :error)
    (read in)))

;(write-to-file run-result "./run-result.lisp")
(setf *run* (read-file "./run-result.lisp"))

;; sort test runs in the database according some criteria
(setf (getf *db* :runs)
      (sort (getf *db* :runs)
            (plist-comparator :lisp #'string<)
            :key #'first))

;; generate report    
(with-open-file (out "D:\\cl-test-grid\\report.html"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (write-sequence (fmt-report (summary-table-html)) out))

;; generate fake database content to test reporting
(setf (getf *db* :runs) (generate-fake-run-results))

(in-package :test-grid)

(with-open-file (out "D:\\cl-test-grid\\results.csv"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
(export-to-csv out))




