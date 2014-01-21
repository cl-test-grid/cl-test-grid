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

           ;; the directory where temporary files
           ;; used to interact with child lisp processes
           ;; are to be stored
           #:*temp-dir*
           ))


(in-package #:lisp-exe)

(defgeneric run-with-timeout (timeout-seconds lisp-exe &rest forms)
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

If the lisp porcess does not finish in the specified TIMEOUT-SECONDS,
the process is killed together with it's possible child processes, a
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

;; dependencies: tg-utils, log4cl, trivial-features, external-program... anything else?

(defclass process ()
  ((native-process :reader native-process
                   :initarg :native-process
                   :initform (error ":native-process is required"))
   (script :reader script
           :initarg :script
           :initform nil)))

(defun process-id (process)
  ;; on CCL external-program:process-id returns process handle
  ;; instead of process id. See http://trac.clozure.com/ccl/ticket/983.
  #+(and ccl windows)
  (lisp-exe-ccl::win-process-handle-to-id (external-program:process-id (native-process process)))
  ;; we haven't tested on other lisps, but hope it will be the process id
  #-(and ccl windows)
  (external-program:process-id (native-process process)))

(defun cleanup (process)
  (let ((script (script process)))
    (when (and script (probe-file script))
      (handler-case (delete-file script)
        (file-error (e)
          (log:warn "Error deleting temporary script ~A: ~A" script e))))))

;; escaping of parameters passed to
;; external-program:run is not required
;; by the external-program specification,
;; but sometimes necessary due to implementations
;; bugs, like http://trac.clozure.com/ccl/ticket/858.
(defun escape-process-parameter (param-str)
  (if (and (member :windows *features*)
           (member :ccl *features*)
           ;; the bug is fixed in CCL 1.9
           ;; :ccl-1.9 will be present in *features* even
           ;; in later versions, like 1.10, 1.11
           (not (member :ccl-1.9 *features*)))
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
(defgeneric make-command-line (lisp-exe form-strings)
 (:documentation "Returns a list of strings. The first string is the
command, the rest strings are the command arguments."))

(defun code-str (lisp-code)
  "Formats lisp code so that it can be read back by lisp reader."
  (let ((*print-case* :downcase))
    (prin1-to-string lisp-code)))

(defmethod start-lisp-process ((lisp-exe lisp-exe) &rest forms)
  (let ((command-line (make-command-line lisp-exe (mapcar #'code-str forms))))
    (make-instance 'process
                   :native-process (start (first command-line)
                                          (rest command-line)))))

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
          "--eval" "(require :abcl-contrib)"
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
          "-ee" ,(escape-process-parameter-for-allegro "(excl:exit 0)"))))

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
                      (external-program:process-status (native-process lisp-process))))
         (return (external-program:process-status (native-process lisp-process))))
       (let ((now (get-universal-time)))
         (when (> now (+ last-active-time 7))
           (signal 'hibernation-detected)
           (incf end-time (- now last-active-time)))
         (setf last-active-time now)
         (when (< end-time now)
           (signal 'lisp-process-timeout :seconds seconds)
           (return)))
       (sleep 1))))

(defun windows-proc-tree-kill-command (process)
  (list "taskkill" "/F" "/T" "/PID" (prin1-to-string (process-id process))))

(defun unix-proc-tree-kill-command (process)
  (list "/bin/sh"
        (asdf:system-relative-pathname :test-grid-agent "agent/killproctree.sh")
        (prin1-to-string (process-id process))
        "9"))

(defun unix-kill-process (process)
  (handler-case
      (external-program:signal-process (native-process process) 9)
    (serious-condition (c)
      ;; If the process is already terminated, when we ask CCL to kill it,
      ;; CCL signals a SIMPLE-ERROR "No such process".
      ;; see http://trac.clozure.com/ccl/ticket/1015
      ;;
      ;; Handle serious conditions, check the process status.
      ;; If it is already finished, just continue. If the status
      ;; is :running, log a warning and continue anyway,
      ;; because we know that CCL do not synchronize
      ;; the process status immediately, so the :running
      ;; value may be just outdated information.
      (let ((status (external-program:process-status (native-process process))))
        (when (eq :running status)
          (log:warn "~A \"~A\" is signalled when killing the process. external-program:process-status still returns :running for the process, but this value may be outdated, so we hope that the error is signalled because the process is already terminated."
                    (type-of c) c))))))

(defun try-to-kill-process-tree (process)
  (log:info "Trying to kill the process tree of ~A" process)
  (let ((command-line (if (member :windows *features*)
                          (windows-proc-tree-kill-command process)
                          ;; does non-windows always mean unix-like? :)
                          (unix-proc-tree-kill-command process))))
    (multiple-value-bind (status exit-code)
        (exec (car command-line) (cdr command-line))
      (when (not (and (eq :exited status)
                      (= 0 exit-code)))
        (log:warn "The result of the process tree kill command is ~A, ~A. Probably the process tree is not killed"
                  status exit-code)
        (unless (member :windows *features*)
          (log:info "As the process tree kill failed, fallback to just killing the process by signal 9...")
          (unix-kill-process process))))))

(defmethod run-with-timeout (timeout-seconds lisp-exe &rest forms)
  (let ((p (apply #'start-lisp-process lisp-exe forms)))
    (unwind-protect
         (handler-case (wait timeout-seconds p)
           (lisp-process-timeout (c)
             (log:warn "Lisp process ~A ~S exceeded the timeout of ~A seconds. Trying to kill the process and its possible child processes" lisp-exe forms timeout-seconds)
             (try-to-kill-process-tree p)
             (signal c)))
      (cleanup p))))

(defparameter *temp-dir* nil)

(defun temp-file (template)
  "TEMPLATE must be a format string with one ~A."
  (let* ((file-name (format nil template (random #.(1- (expt 2 64))))))
    (if *temp-dir*
        (merge-pathnames file-name *temp-dir*)
        (let ((*package* (find-package :keyword)))
          (log:warn "~S is not set, temporary file ~A will be created in the default directory."
                    '*temp-dir* file-name)
          file-name))))

(defmethod start-lisp-process ((lisp-exe lispworks) &rest forms)
  ;; the :windows feature is put into *features* by trivial-features
  (if (not (member :windows *features*))
      (call-next-method)
      ;; On Windows we pass Lisp code via temporary file,
      ;; because LispWorks command line parser is buggy:
      ;; https://groups.google.com/d/msg/cl-test-grid/VH4oHb47riU/blEPk2M5G2EJ
      (let* ((script (temp-file "script~A.lisp")))
        (log:info "Writing code ~{~S~} to temporary file ~A" forms script)
        (with-open-file (stream (ensure-directories-exist script)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type tg-utils::*utf-8-compatible-character-type*
                                :external-format tg-utils::*utf-8-external-format*)
          (write-string "(win32:dismiss-splash-screen t)" stream)
          (terpri stream)
          (write-string "(load-all-patches)" stream)
          (terpri stream)
          (format stream "~{~S~%~}" forms)
          (write-string "(lw:quit)" stream))
        (let ((native-process (start (exe-path lisp-exe)
                                     `("-init" ,(namestring (truename script)) "-siteinit" "-"))))
          (make-instance 'process
                         :native-process native-process
                         :script script)))))
