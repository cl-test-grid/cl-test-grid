(defpackage #:nst-api-impl
  (:use #:cl :nst-api))

(in-package #:nst-api-impl)

(defun run-test-suite (test-suite-name)
  (format t "Running test suite ~A~%" test-suite-name)
  (nst::run-command-actual :clear)
  (let ((*print-pretty* t))
    (if (find-package (symbol-name test-suite-name))
        (nst::run-command-actual :run-package test-suite-name)
        (nst::run-command-actual :run test-suite-name)))
  (let ((report (nst::report-interesting)))
    (when report
      (nst::multi-results-package-reports
       (nst::multi-results-stats-source report)))))

(defun run-test-suites (&rest test-suite-names)
  (mapcan #'run-test-suite test-suite-names))

(defun maphash* (fn hash)
  (let (results)
    (maphash #'(lambda (k v)
                 (push (funcall fn k v) results))
             hash)
    results))

(defun bad-tests? (results)
  (or (plusp (slot-value results 'nst::failing))
      (plusp (slot-value results 'nst::erring))))

(defun make-note-formatter (package-name group-name test-name note-type)
  (declare (ignore note-type))
  (lambda (note)
    (declare (ignore note))
    (string-downcase (format nil "~A::~A::~A"
                             package-name
                             group-name
                             test-name))))

(defun make-test-result-formatter (package-name group-name)
  (lambda (test-name test-results)
    (when (bad-tests? test-results)
      (flet ((make-note-formatter (note-type)
               (make-note-formatter package-name group-name
                                    test-name note-type)))
        (let ((failures (nst::check-result-failures test-results))
              (errors (nst::check-result-errors test-results)))
          (append (mapcar (make-note-formatter :failure) failures)
                  (mapcar (make-note-formatter :error) errors)))))))

(defun make-group-result-formatter (package-name)
  (lambda (group-name group-results)
    (when (bad-tests? group-results)
      (let ((formatter (make-test-result-formatter package-name group-name))
            (test-results (nst::group-result-check-results group-results)))
        (apply #'nconc (maphash* formatter test-results))))))

(defun format-package-result (package-result)
  (let* ((package-name (nst::package-result-package-name package-result))
         (formatter (make-group-result-formatter package-name))
         (group-results (nst::package-result-group-results package-result)))
    (when (bad-tests? package-result)
      (apply #'nconc (maphash* formatter group-results)))))

(defun failed-tests (test-suite-results)
  (mapcan #'format-package-result test-suite-results))
