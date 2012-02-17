;;;; An utility to help submitting test results after
;;;; the Lisps where drakma doesn't work (today it doesn't work
;;;; on several Lisps).
;;;;
;;;; After the test result submittion fails, we can run
;;;; this utility on another Lisp, where drakma isn't broken
;;;; and it submits all the test results for the last 24 horus
;;;; which haven't been submitted yet.
;;;;
;;;; This code depends on cl-fad for file system operations,
;;;; therefore we do not include it into test-grid.lisp,
;;;; as we don't want test-grdid.lisp to depend on anything
;;;; other than ASDF and Quicklisp.

(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))
  (pushnew this-file-dir asdf:*central-registry* :test #'equal))

(asdf:operate 'asdf:load-op :test-grid)

(ql:quickload "cl-fad")

(defun submitted-p (test-run-directory)
  "Tests the specified TEST-RUN-DIRECTORY
whether the test results are submitted by examining
the rist lib-result in the test-run-info.lisp file
containing there and checking if the first lib-result here
has a blobstore key for the library log."
  (let* ((run-info-file (test-grid::run-info-file test-run-directory))
         (run-info (and (cl-fad:file-exists-p run-info-file)
                        (test-grid::safe-read-file run-info-file))))
    (getf (first (getf run-info :results))
          :log-blob-key)))

(defun submit-last-day-results ()
  (let ((hour-ago-str (test-grid::fmt-time (- (get-universal-time)
                                              #.(* 60 60 24))))
        (submit-count 0))

    (dolist (test-dir (cl-fad:list-directory (test-grid::test-output-base-dir)))
      (let ((dir-name (file-namestring (cl-fad:pathname-as-file test-dir))))
        (when (and (string> dir-name hour-ago-str)
                   (not (submitted-p test-dir)))
          (format t "submitting ~A~%" test-dir)
          (test-grid::submit-results test-dir)
          (incf submit-count))))
    (format t "~A test results submitted~%" submit-count)
    submit-count))

(submit-last-day-results)

