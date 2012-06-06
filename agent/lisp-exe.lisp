;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-agent)

(defclass lisp-exe () ())
(defgeneric run-lisp-process (lisp-exe &rest forms)
  (:documentation "Starts lisp process, executes the specified forms
and exits the process."))

;; excaping of parameters passed to
;; external-program:run is not required
;; by the external-program specification,
;; but sometimes necessary due to implementations
;; bugs, like http://trac.clozure.com/ccl/ticket/858.
(defun escape-process-parameter (param-str)
  (if (and (member :windows *features* :test #'eq)
           (member :ccl *features* :test #'eq))
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

(defclass single-exe-lisp-exe (lisp-exe)
  ((exe-path :type string
             :accessor exe-path
             :initarg :exe-path
             :initform (error "exe-path must be specified"))))

(defun code-str (lisp-code)
  "Formats lisp code so that it can be read back by lisp reader."
  (prin1-to-string lisp-code))

(defun prepend-each (prepend-what list)
  (mapcan (lambda (elem) (list prepend-what elem))
          list))

(assert (equal (prepend-each "--eval" '(1 2))
               '("--eval" 1 "--eval" 2)))

(defclass clisp (single-exe-lisp-exe) ())
(defmethod run-lisp-process ((lisp-exe clisp) &rest forms)
  (exec (exe-path lisp-exe)
        `("-norc"
          "-m" "100MB"
          ,@(prepend-each "-x" (mapcar #'code-str forms)))))

(defclass ccl (single-exe-lisp-exe) ())
(defmethod run-lisp-process ((lisp-exe ccl) &rest forms)
  (exec (exe-path lisp-exe)
        `("--no-init"
          ,@(prepend-each "--eval" (mapcar #'code-str forms))
          "--eval" "(ccl:quit)")))

(defclass abcl (lisp-exe) 
  ((java-exe-path :type string
                  :accessor java-exe-path
                  :initarg :java-exe-path
                  :initform (error "java-exe-path must be specified"))
   (abcl-jar-path :type string
                  :accessor abcl-jar-path
                  :initarg :abcl-jar-path
                  :initform (error "abcl-jar-path must be specified"))))

(defmethod run-lisp-process ((lisp-exe abcl) &rest forms)
  (exec (java-exe-path lisp-exe)
        `("-XX:MaxPermSize=256m"
          "-jar"
          ,(abcl-jar-path lisp-exe)
          "--noinit"
          "--nosystem"
          "--batch"
          ,@(prepend-each "--eval" (mapcar #'code-str forms)))))

(defclass sbcl (single-exe-lisp-exe) ())
(defmethod run-lisp-process ((lisp-exe sbcl) &rest forms)
  (exec (exe-path lisp-exe)
        `("--noinform"
          "--end-runtime-options"
          "--no-sysinit"
          "--no-userinit"
          ,@(prepend-each "--eval" (mapcar #'code-str forms))
          "--eval" "(sb-ext:quit)")))

(defclass cmucl (single-exe-lisp-exe) ())
(defmethod run-lisp-process ((lisp-exe cmucl) &rest forms)
  (exec (exe-path lisp-exe)
        `("-noinit"
          "-nositeinit"
          ,@(prepend-each "-eval" (mapcar #'code-str forms))
          "-eval" "(quit)")))

(defclass ecl (single-exe-lisp-exe) ())
(defmethod run-lisp-process ((lisp-exe ecl) &rest forms)
  (exec (exe-path lisp-exe)
        `("-norc"
          ,@(prepend-each "-eval" (mapcar #'code-str forms))
          "-eval" "(ext:quit)")))

(defclass acl (single-exe-lisp-exe) ())

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

(defmethod run-lisp-process ((lisp-exe acl) &rest forms)
  ;; docs: http://www.franz.com/support/documentation/6.2/doc/startup.htm#command-line-args-1
  (exec (exe-path lisp-exe)
        `(#+windows "+c" ;; no console window
          #+windows "+B" ;; no splash screen
          "-qq" ;; don't read any initialization files
          "-batch"
          ,@(prepend-each "-ee" (mapcar (alexandria:compose #'escape-process-parameter-for-allegro 
                                                            #'code-str)
                                        forms))
          "-ee" "(excl:exit 0)")))

;; Lispwork is not tested as I don't have a license,
;; and the free personal edition doesn't have
;; a command line executable, only GUI.
(defclass lispworks (single-exe-lisp-exe) ())
(defmethod run-lisp-process ((lisp-exe lispworks) &rest forms)
  ;; docs: http://www.lispworks.com/documentation/lw60/LW/html/lw-484.htm
  (exec (exe-path lisp-exe)
        `("-init" "-"     ;; don't read
          "-siteinit" "-" ;; any initialization files
          ,@(prepend-each "-eval" (mapcar #'code-str forms))
          "-eval" "(lispworks:quit)")))
