;;;; User (my) session

;; In my system CLISP doesn't execute .clisprc when started
;; from SLIME, therefore quicklisp is not available
#+clisp 
(load "/Users/anton/.clisprc")

(pushnew "D:/cl-test-grid/" asdf:*central-registry*)

(pushnew "C:/Users/anton/projects/cl-test-grid/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op :test-grid)

(setf test-grid::*gae-blobstore-base-url* "http://localhost:8080")

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

(add-run run-result *db*)

(save-db *db*)

(generate-reports *db*)

;; generate fake database content to test reporting
(setf (getf *db* :runs) (generate-fake-run-results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; developer experiments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sort test runs in the database according some criteria
(setf (getf *db* :runs)
      (sort (getf *db* :runs)
            (plist-comparator :lisp #'string<)
            :key #'first))

