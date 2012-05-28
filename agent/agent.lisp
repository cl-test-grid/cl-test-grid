(in-package #:test-grid-agent)

(defclass agent () 
   ;; The list of lisp-impl's to run tests on.
  ((lisps :type list :accessor lisps :initform nil)
   ;; The lisp-impl considered as more reliable on this OS, 
   ;; and supporting more libraries. Used run various small
   ;; lisp programs like quicklisp update.
   (preferred-lisp :type (or null lisp-impl) :accessor preferred-lisp :initform nil)
   (user-email :type (or null string) :accessor user-email :initform nil)))

;;; Working directory structure
(defun work-dir ()
  (merge-pathnames "work-dir/agent/"
                   test-grid-config::*src-base-dir*))

(defun test-output-base-dir ()
  (merge-pathnames "test-runs/"
                   (work-dir)))

(defun workdir-file (relative-path)
  (merge-pathnames relative-path (work-dir)))

;;; File system location of our source code
(defun src-dir ()
  (merge-pathnames "agent/"
                   test-grid-config::*src-base-dir*))

(defun src-file (file-name)
  (merge-pathnames file-name (src-dir)))

;;; Config file
(defparameter +config-file-name+ "cl-test-grid-config.lisp")

(defun config-file()
  (merge-pathnames (user-homedir-pathname) +config-file-name+))

;;; configure log4cl - daily rolling file
(defun log-file ()
  ;; good thing about log4cl, it creates
  ;; intermediate directories automatically,
  ;; so we need not care about this
  (merge-pathnames "logs/agent.log"
                   (work-dir)))

;; sketch of the procedure to implement:
;; Level 1
;; - ensure quicklisp is updated to the lates version
;; - run the tests and upload results
;; Level 2
;; - ensure quicklisp is updated to the version we need to test
;; - ask each lisp for it's name
;; - retrieve (either from the local presistence or from server)
;;   the list of libraries which have not beeen tested
;;   on this quicklisp and on our lisps
;; - run the tests and upload results (and mark the
;;   libraries as being tested so that we do not
;;   repeat the tests)

;; TODO:
;; * Level 1 beta
;; + test runs in work-dir/agent/test-runs
;; - configuration: configure list of lisps, user email. 
;; - configuration: test-config procedcure (try to run all the lisps configured)
;; - configuration: contributor email as a configurabel agent propery,
;;   instead of cl-test-grid-settings.lisp
;; - delete test run data after the results have been submitted
;; * Level 1 stable
;; + full path to files: setup, test-run-process, response-file
;; - timeout
;; - check status of the quicklisp update process
;; - Prevent lisp entering debugger.
;; - Different lisps treat unhangled signals during -eval
;;   differently: ECL exits with status 1, CCL enters debugger
;;   and hangs.
;; - When signalling absense of the config file, provide
;;   a fully commented example config file so that user can easily 
;;   fix it.
;; - In the config file, should user work in the test-grid-agent
;;   package, or in cl-user package and use a public API of
;;   test-grid-agent?
;; - program parameters escaping is not perfect. When we
;;   run CLISP as an external process, it can not stand
;;   string literals with " inside.
;; - enable/disable program parameters escapting depending on the
;;   external-program behaviour (consider also using input stream
;;   of the lisp process, or a temporary file)
;; - prevent test run directory names conflict (currently they
;;   are named by timestamp with resolution to seconds)
;; - temp file naming: ensure unique [probably specify random-state]
;; * Level 2
;; - do we need node-id (or agent-id)? should it be
;;   meaningfull string provided by user, or an 
;;   automatically generated one? Note, this value is different
;;   from the contributor email, because one contributor
;;   may have more than one machine.
;; * Very stable 
;; - ECL needs C compiler, how to ensure it is available (can we specify
;;   it explicitly when configurin lisp-impl for ECL or assume
;;   it is available in path)?
;; * Backward compatibility
;; - remove the old test-runs directory
;; - remove the old ~/cl-test-grid-settings.lisp
;;   (or rename it to ~/cl-test-grid-settings.lisp.unused?)

;;; When we run lisp code in an external lisp process,
;;; and want to return some value form that process,
;;; we use a temporary file where the external process
;;; stores the response, and we READ that response
;;; from the file.
(defun with-response-file-impl (body-func)
  (let* ((response-file-name (format nil
                                     "response~A.lisp"
                                     (random #.(1- (expt 2 64)))))
         (response-file (workdir-file response-file-name)))
    (unwind-protect (progn (funcall body-func response-file)
                           (test-grid::safe-read-file response-file))
      (when (probe-file response-file)
        (delete-file response-file)))))

(defmacro with-response-file ((file-var) &body body)
  `(with-response-file-impl (lambda (,file-var) ,@body)))

;;; Primary functions of test-grid agent
(defun run-tests-in-separate-process (agent lisp-impl test-suites)
  "Runs the specified test suites in a separate process
and returns the directory where the test results are saved."
  (with-response-file (response-file)
    (let* ((code `(progn
                    (load ,(workdir-file "quicklisp/setup.lisp"))
                    (load ,(src-file "test-run-process.lisp"))
                    (cl-user::run-with-response-to-file (quote ,test-suites)
                                                        ,(test-output-base-dir)
                                                        ,(user-email agent)
                                                        ,response-file))))
      (log:info "preparing to start separate lisp process with code: ~S" code)
      (run-lisp-process lisp-impl code))))

(defun update-testing-quicklisp (lisp-impl)
  (log:info "Ensuring the quicklisp used to download the libraries being tested is updated to the recent version...")
  (run-lisp-process lisp-impl
                    `(load ,(truename (src-file "ensure-quicklisp-updated.lisp"))))
  (log:info "Quicklisp update process finished."))

(defun load-config ()
  (let ((config-file (config-file)))
    (unless (probe-file config-file)
      (error "Configuration file ~A is absent, can not run test-grid agent." config-file))
    (let ((*package* (find-package '#:test-grid-agent)))
      (load config-file))))

;;; Configuration check functions
(defun lisp-process-echo (lisp-impl str-to-echo)
  (with-response-file (response-file)
    (run-lisp-process lisp-impl 
                      `(with-open-file (cl-user::out ,response-file
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                         (pprint ,str-to-echo cl-user::out)))))

(defun check-lisp (lisp-impl)
  (let* ((echo-string "abcs *() { /d e11 ")
         (response (lisp-process-echo lisp-impl echo-string)))    
    (when (not (string= echo-string response))
      (error "Lisp ~A returned string ~S while we exected ~S."
             lisp-impl response echo-string))))

(defun check-config (agent)
  (log:info "Checking configuration...")
  (when (zerop (length (user-email agent)))
    (error "The user-email property of test-grid:*agent* can not be empty. Please specify your email, or at least some nickname if you are strongly opposed to publish your email."))
  (log:info "Checking external process functionality for the preffered lisp ~A..."
            (preferred-lisp agent))
  (check-lisp (preferred-lisp agent))
  (dolist (lisp (lisps agent))
    (log:info "Checking external process functionality for ~A..." lisp)
    (check-lisp lisp)
    (log:info "~A OK" lisp))
  (log:info "All the external lisps configuration checked OK")
  t)

;;; split a list into sublists by n elements,
;;; e.g. (a b c d e) by 3 => (a b c) (d e)
(defclass list-splitter ()
  ((remainder :type list 
              :accessor remainder
              :initarg :list
              :initform (error ":list is required"))))

(defun next (list-splitter &optional (n 1))
  (let ((result nil))
    (dotimes (i n)
      (when (null (remainder list-splitter))
        (return-from next (nreverse result)))
      (push (car (remainder list-splitter)) result)
      (setf (remainder list-splitter)
            (cdr (remainder list-splitter))))
    (nreverse result)))

(let ((ls (make-instance 'list-splitter :list '(1 2 3))))
  (assert (equal '(1 2) (next ls 2)))
  (assert (equal '(3) (next ls 2)))
  (assert (equal nil (next ls 2))))

(defun split-list (list n)
  (let ((splitter (make-instance 'list-splitter :list list))
        (result nil))
    (loop 
       (let ((sub (next splitter n)))
         (when (null sub)
           (return (nreverse result)))
         (push sub result)))))

(assert (equal '((1 2 3) (4 5))
               (split-list '(1 2 3 4 5) 3)))

(defun run-tests (agent)
  (dolist (lisp (lisps agent))
    (log:info "Running tests for ~A" lisp)
    (dolist (lib-block (split-list test-grid::*all-libs* 8))
      (log:info "libraries block to test in a single process: ~S" lib-block)
      (let ((results-dir (run-tests-in-separate-process agent lisp lib-block)))
        (test-grid::submit-results results-dir)
        (cl-fad:delete-directory-and-files results-dir :if-does-not-exist :ignore)))))

;;; Main program

(defparameter *agent* nil
  "The AGENT instance. This variable is provided in order
to make the agent accessible from the user-edited config
file cl-test-grid-config.lisp.")

(defun main ()
  (log:config :daily (log-file))
  (let ((*agent* (make-instance 'agent)))
    (load-config)
    (check-config *agent*)
    (update-testing-quicklisp (preferred-lisp *agent*))
    (run-tests *agent*)))


;(update-testing-quicklisp *ccl-18-32*)

;;(run-tests-in-separate-process *acl* (list :babel))


;;  (time
;;   (dolist (test-suite test-grid::*all-libs*)
;;     (run-tests-in-separate-process *acl* (list test-suite))))

;; ===================================

;; (external-program:run "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl.exe"
;;                       '("--no-init"
;;                         "--load" "quicklisp/setup.lisp"
;;                         "--load" "run-agent.lisp"
;;                         "--eval" "\"(test-grid::run-libtests '(:babel))\""
;;                         "--eval" "(quit)"))

;; (external-program:run "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl.exe"
;;                       '("--no-init"
;;                         "--load" "quicklisp/setup.lisp"
;;                         "--load" "run-agent.lisp"
;;                         "--eval" "(test-grid::run-libtests '(:iterate))"
;;                         "--eval" "(quit)"))
