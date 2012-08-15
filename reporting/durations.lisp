(in-package #:test-grid-reporting)

(defun print-test-run-durations (lib-world)
  (dolist (run (getf (test-grid-data::read-db)
                     :runs))
    (let ((descr (test-grid-data::run-descr run)))
      (when (string= lib-world (getf descr :lib-world))
        (format t "~A ~A~%" 
                (getf descr :lisp)
                (float (/ (getf descr :run-duration)
                          60)))))))

;; usage:
;; (print-test-run-durations "quicklisp 2012-07-03")

(defun print-contributors (lib-world)
  (dolist (run (getf (test-grid-data::read-db)
                     :runs))
    (let ((descr (test-grid-data::run-descr run)))
      (when (string= lib-world (getf descr :lib-world))
        (format t "~A ~A~%" 
                (getf descr :lisp)
                (getf (getf descr :contact) :email))))))

(print-contributors "quicklisp 2012-07-03")
