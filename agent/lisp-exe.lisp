;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; This package provides means to run lisp code in separate
;;;; process for various Common Lisp implementatons. The API
;;;; serves only the minimal needs sufficient for test-grid agent.
;;;; We do not try to formulate generic API suitable for 3rd party
;;;; projects (although useful implementation pieces may probably
;;;; be found in this package would someone need similar functionality).

;;;
;;; Interface
;;;

(defpackage #:lisp-exe
  (:use #:cl)
  (:export #:lisp-exe ;; the base class for a CL implementation executable

           ;; lisp-exe classes for particular CL implementations
           ;; which we know how to run
           #:clisp
           #:ccl
           #:abcl
           #:java-exe-path ; abcl slot accessor
           #:abcl-jar-path ; abcl slot accessor
           #:sbcl
           #:cmucl
           #:acl
           #:ecl
           #:compiler ; ecl slot accessor
           #:lispworks

           ;; the main function of interest for test-grid agent
           #:run-with-timeout
           #:lisp-process-timeout ; timeout condition
           #:seconds              ; the condition slot accessor
           #:hibernation-detected ; another condition

            ;; deprecated function, consider run-with-timeout where possible
           #:run-lisp-process
           ))


(in-package #:lisp-exe)

(defgeneric run-lisp-process (lisp-exe &rest forms)
  (:documentation "Starts lisp process, executes the specified forms
and exits the process. What happens in case of errors in forms (syntax or runtime errors),
entering debugger and other problems is not specified. For example some lisps
just exit in case debugger is entered in batch mode, other lisps really
enter interactive debugger and hang waiting for input. The function doesn't
even allow to test exit status of the process. It is responsibility
of the lisp code in FORMS to provide handling of problematic situations
and deliver response to the parent process (e.g. by storing the result
value in a temporary file - if the temporary file is absent the parent
knows something is wrong).

This function is deprecated because it does not allow to handle
hanging lisp processes. Consider RUN-WITH-TIMEOUT where possible."))

(defgeneric run-with-timeout (timeout-seconds lisp-exe &rest forms)
  (:documentation "Like RUN-LISP-PROCESS, but if the lisp porcess
does not finish in the specified TIMEOUT-SECONDS, the process
is killed together with it's possible child processes, a
LISP-PROCESS-TIMEOUT condition is signalled and the function returns NIL.

If while waiting for the process to finish it is detected that
the machive was hibernated and then woken up, HIBERNATION-DETECTED
conditon is signalled (which is not a SERIOUS-CONDITION subclass),
and the hibernation duration is excluded from the time accounted
for the timeout.

If the calller thinks the hibernation may affect the results
of the lisp process (e.g. because network connections
were terminated) he can handle the HIBERNATION-DETECTED
condition; for example by running the process again.
Otherwise he may just ignore the condition."))

(define-condition lisp-process-timeout (condition) ;; should it inherit from ERROR?
  ((seconds :accessor seconds
            :initarg :seconds
            :type number
            :initform (error ":seconds is required"))))

(define-condition hibernation-detected (condition) ())

(defclass lisp-exe () ())

;;; lisp-exe classes for particlar lisps
(defclass single-exe-lisp-exe (lisp-exe)
  ((exe-path :type string
             :accessor exe-path
             :initarg :exe-path
             :initform (error "exe-path must be specified"))))

(defclass clisp (single-exe-lisp-exe) ())
(defclass ccl (single-exe-lisp-exe) ())
(defclass abcl (lisp-exe)
  ((java-exe-path :type string
                  :accessor java-exe-path
                  :initarg :java-exe-path
                  :initform (error "java-exe-path must be specified"))
   (abcl-jar-path :type string
                  :accessor abcl-jar-path
                  :initarg :abcl-jar-path
                  :initform (error "abcl-jar-path must be specified"))))

(defclass sbcl (single-exe-lisp-exe) ())
(defclass cmucl (single-exe-lisp-exe) ())

;; ECL provides two compilers: bytecode and lisp-to-c.
;; What of two compilers to use should be specified
;; explicitly as a constructor paraemter.
;;
;; Note, .fas files already compiled are loaded
;; if found by cl:load, even if they are compiled
;; by different compiler that is currently enabled.
;; Therefore you need to preform fresh
;; recomplilation of the code if you want to be sure
;; you are running the code really compiled by the
;; compiler you specified.
(defclass ecl (single-exe-lisp-exe)
  ((compiler :type symbol
             :accessor compiler
             :initarg :compiler
             :initform (error "compiler must be either :bytecode or :lisp-to-c. Can not be NIL"))))

(defclass acl (single-exe-lisp-exe) ())
;; Lispwork was not verified, as I don't have a license,
;; and the free personal edition doesn't have
;; a command line executable, only GUI.
(defclass lispworks (single-exe-lisp-exe) ())


;;;
;;; Implementation
;;;

;; excaping of parameters passed to
;; external-program:run is not required
;; by the external-program specification,
;; but sometimes necessary due to implementations
;; bugs, like http://trac.clozure.com/ccl/ticket/858.
(defun escape-process-parameter (param-str)
  (if (and (member :windows *features*)
           (member :ccl *features*))
      (with-output-to-string (s)
        (princ #\" s)
        (loop for ch across param-str
           do (progn
                (when (member ch '(#\" #\\) :test #'char=)
                  (princ #\\ s))
                (princ ch s)))
        (princ #\" s))
      param-str))

;; small wrapper around external-program:run
(defun exec (program-path argument-strings)
  (let ((args (mapcar #'escape-process-parameter argument-strings)))
    (log:info "running command: ~A ~{~A~^ ~}" program-path args)
    (external-program:run program-path args)))

;; small wrapper around external-program:start
(defun start (program-path argument-strings)
  (let ((args (mapcar #'escape-process-parameter argument-strings)))
    (log:info "starting command: ~A ~{~A~^ ~}" program-path args)
    (external-program:start program-path args :input t)))

(defgeneric start-lisp-process (lisp-exe &rest forms))

;;; default implementation of start-lisp-process
(defgeneric make-command-line (lisp-exe from-strings)
 (:documentation "Returns a list of strings. The first string is the
command, the rest strings are the command arguments."))

(defun code-str (lisp-code)
  "Formats lisp code so that it can be read back by lisp reader."
  (prin1-to-string lisp-code))

(defmethod start-lisp-process ((lisp-exe lisp-exe) &rest forms)
  (let ((command-line (make-command-line lisp-exe (mapcar #'code-str forms))))
    (start (first command-line) (rest command-line))))

(defmethod run-lisp-process ((lisp-exe lisp-exe) &rest forms)
  (let ((command-line (make-command-line lisp-exe (mapcar #'code-str forms))))
    (exec (first command-line) (rest command-line))))

(defun prepend-each (prepend-what list)
  (mapcan (lambda (elem) (list prepend-what elem))
          list))

(assert (equal (prepend-each "--eval" '(1 2))
               '("--eval" 1 "--eval" 2)))

(defmethod make-command-line ((lisp-exe clisp) form-strings)
  (cons (exe-path lisp-exe)
        `("-norc"
          "-m" "100MB"
          ,@(prepend-each "-x" form-strings))))

(defmethod make-command-line ((lisp-exe ccl) form-strings)
  (cons (exe-path lisp-exe)
        `("--no-init"
          ,@(prepend-each "--eval" form-strings)
          "--eval" "(ccl:quit)")))

(defmethod make-command-line ((lisp-exe abcl) form-strings)
  (cons (java-exe-path lisp-exe)
        `("-XX:MaxPermSize=256m"
          "-jar"
          ,(abcl-jar-path lisp-exe)
          "--noinit"
          "--nosystem"
          "--batch"
          ,@(prepend-each "--eval" form-strings))))

(defmethod make-command-line ((lisp-exe sbcl) form-strings)
  (cons (exe-path lisp-exe)
        `("--noinform"
          "--end-runtime-options"
          "--no-sysinit"
          "--no-userinit"
          ,@(prepend-each "--eval" form-strings)
          "--eval" "(sb-ext:quit)")))

(defmethod make-command-line ((lisp-exe cmucl) form-strings)
  (cons (exe-path lisp-exe)
        `("-noinit"
          "-nositeinit"
          ,@(prepend-each "-eval" form-strings)
          "-eval" "(quit)")))

(defmethod make-command-line ((lisp-exe ecl) form-strings)
  (cons (exe-path lisp-exe)
        `("-norc"
          "-eval" ,(ecase (compiler lisp-exe)
                          (:bytecode "(ext::install-bytecodes-compiler)")
                          (:lisp-to-c "(require :cmp)"))
          ;; we may replace (require :cmp) by (ext:intall-c-compiler),
          ;; but it is relatevely new function and is absent in old versions
          ;; of ECL. Also ext::install-bytecodes-compiler is not
          ;; exported from the ext package in old ECL versions.
          ,@(prepend-each "-eval" form-strings)
          "-eval" "(ext:quit)")))

;; escapes parameters for the -ee option of Allegro,
;; as described here:
;; http://www.franz.com/support/documentation/6.2/doc/startup.htm#spec-ee-note
;;
;; Why we need this: because on Windows, even if we include
;; the parameter into quotas, and escape the inner qoutas
;; by \, Allegro recevies the full command line including
;; all the quotas (i.e. the quotas are unescaped back
;; by external-program or by Windows). And in result
;; Allegro can't read the parameter propertly.
(defun escape-process-parameter-for-allegro (param)
  (with-output-to-string (s)
    (loop for char across param
       do (if (and (< (char-code char) 256)
                   (not (alphanumericp char)))
              (format s "%~(~2,'0x~)" (char-code char))
              (princ char s)))))

(defmethod make-command-line ((lisp-exe acl) form-strings)
  ;; docs: http://www.franz.com/support/documentation/6.2/doc/startup.htm#command-line-args-1
  (cons (exe-path lisp-exe)
        `(#+windows "+c" ;; no console window
          #+windows "+B" ;; no splash screen
          "-qq" ;; don't read any initialization files
          "-batch"
          ,@(prepend-each "-ee" (mapcar #'escape-process-parameter-for-allegro
                                        form-strings))
          "-ee" "(excl:exit 0)")))

(defmethod make-command-line ((lisp-exe lispworks) form-strings)
  (log:warn "lisp-exe for lispworks hasn't been tested, may fail; creating command line for lispworks...")
  ;; docs: http://www.lispworks.com/documentation/lw60/LW/html/lw-484.htm
  (cons (exe-path lisp-exe)
        `("-init" "-"     ;; don't read
          "-siteinit" "-" ;; any initialization files
          ,@(prepend-each "-eval" form-strings)
          "-eval" "(lispworks:quit)")))

;;; Timeouts: waiting for the process and killing the process tree on timeout;
;;; also detecting the machine hibernation while the process is running.

(defun wait (seconds lisp-process)
  "If the process is not finished upot the SECONDS timeout
signal LISP-PROCESS-TIMEOUT conditios and exit (the process
remains running)."
  (let* ((last-active-time (get-universal-time))
         (end-time (+ seconds last-active-time)))
    (loop
       (when (not (eq :running
                      (external-program:process-status lisp-process)))
         (return (external-program:process-status lisp-process)))
       (let ((now (get-universal-time)))
         (when (> now (+ last-active-time 7))
           (signal 'hibernation-detected)
           (incf end-time (- now last-active-time)))
         (setf last-active-time now)
         (when (< end-time now)
           (signal 'lisp-process-timeout :seconds seconds)
           (return)))
       (sleep 1))))

(defun try-to-kill-process-tree (lisp-process)
  (log:info "Trying to kill the process tree of ~A" lisp-process)
  (if (member :windows *features*)
      (windows-kill-process-tree lisp-process)
      ;; does non-windows always mean unix-like? :)
      (unix-kill-process-tree lisp-process)))

(defun windows-kill-process-tree (lisp-process)
  (let ((process-id
         ;; on CCL external-program:process-id returns process handle
         ;; instead of process id. See http://trac.clozure.com/ccl/ticket/983.
         #+(and ccl windows)
         (lisp-exe-ccl::win-process-handle-to-id (external-program:process-id lisp-process))
         ;; we haven't tested on other lisps, but hope it will be the process id
         #-(and ccl windows)
         (external-program:process-id p)))
    (multiple-value-bind (status exit-code)
        (exec "taskkill" (list "/F" "/T" "/PID" (prin1-to-string process-id)))
      (when (not (and (eq :exited status)
                      (= 0 exit-code)))
        (log:warn "The result of taskkill unitilty is ~A, ~A for process ID ~A. Probably the process tree is not killed"
                  status exit-code process-id)))))

(defun unix-kill-process-tree (lisp-process)
  ;; On unix killing the whole tree is less important,
  ;; because one of the main motivations for killing
  ;; tree is that CLISP always starts two processes on
  ;; windows: clisp.exe and it's child performing the
  ;; real work - lisp.exe. On unix-like systems
  ;; lisp.exe is started using execv which replaces
  ;; the parent process by child process, so CLISP
  ;; is runnin in a single process.
  ;;
  ;; Still, killing the process tree is desirable,
  ;; in case the test suite starts some other programs
  ;; (as external-program teste sute, which runs some
  ;; shell commands). Implementing the process tree
  ;; kill on unix is in our TODO.
  (log:warn "Killing the process tree for non-windows platforms is not implemented yet. Just killing the process.")
  (handler-case
      (external-program:signal-process lisp-process 9)
    (serious-condition (c)
      ;; If the process already terminates, when we ask CCL to kill it,
      ;; CCL signals a SIMPLE-ERROR "No such process".
      ;; see http://trac.clozure.com/ccl/ticket/1015
      ;;
      ;; Handle serious conditions, check the process status.
      ;; If it is already finished, just continue. If the status
      ;; is :running, log a warning and continue anyway,
      ;; because we know that CCL do not synchronize
      ;; the process status immediately, so the :running
      ;; value may be just outdated information.
      (let ((status (external-program:process-status lisp-process)))
        (when (eq :running status)
          (log:warn "~A \"~A\" is signalled when killing the process. external-program:process-status still returns :running for the process, but this value may be outdated, so we hope that the error is signalled because the process is already terminated."
                    (type-of c) c))))))

(defmethod run-with-timeout (timeout-seconds lisp-exe &rest forms)
  (let ((p (apply #'start-lisp-process lisp-exe forms)))
    (handler-case (wait timeout-seconds p)
      (lisp-process-timeout (c)
        (log:warn "Lisp process ~A ~S exceeded the timeout of ~A seconds. Trying to kill the process and it's possible child processes" lisp-exe forms timeout-seconds)
        (try-to-kill-process-tree p)
        (signal c)))))
