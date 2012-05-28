(in-package #:test-grid-agent)

;; external program SBCL windows problems:
;;
;; 1.
;; (let ((cl:*package* (find-package '#:sb-ext)))
;;   (defun sb-ext:posix-environ () nil))
;;
;; 2. sb-ext:run-program doesn't accept :environment nor :env keyword arguments
;;
;;
;; Other issues: quoting the program arguments against shell interpretation is
;; unappropriate (it's just program arguments, like ecexcve or it's friends).
;; Quoting is necessary because '("--eval" "(+ 1 2)") doesnt work for arguments,
;; while '("--eval" "\"(+ 1 2)\"") works.
;;
;; That's how args are treated on CCL.
;;
;; On CLISP args do not need to be escaped.
;;
;; On CLISP the method external-program:run
;; incorrectly interprets the exit status
;; returned by ext:run-program.
;;
;; The rules of ext:run-program:
;; case 1
;;   if :input or :output were T,
;;   than ext:run-program return value
;;   NIL means the program exited successfully
;;   with status 0. Othersise ext:run-program
;;   returns a positive integer.
;; case 2
;;    if :stream was specified for both
;;    input and output, 3 streams are returned.
;;
;; The external-program:run method interprets
;; the ext:run-program return value absolutelly
;; differently.

(defclass lisp-impl () ())
(defgeneric run-lisp-process (lisp-impl lisp-code))

(defclass single-exe-lisp-impl (lisp-impl)
  ((exe-path :type string
             :accessor exe-path
             :initarg :exe-path
             :initform (error "exe-path must be specified"))))


(defun code-str (lisp-code)
  (with-output-to-string (s)
    (prin1 lisp-code s)))

(defun escape-process-parameter (param-str)
  (with-output-to-string (s)
    (princ #\" s)
    (loop for ch across param-str
         do (progn
              (when (member ch '(#\" #\\) :test #'char=)
                (princ #\\ s))
              (princ ch s)))
    (princ #\" s)))

;; small wrapper around external-program:run
(defun exec (program-path argument-strings)
  (let ((args (mapcar #'escape-process-parameter argument-strings)))
    (log:info "running command: ~A ~{~A~^ ~}" program-path args)
    (external-program:run program-path args)))

(defclass clisp (single-exe-lisp-impl) ())

(defmethod run-lisp-process ((lisp-impl clisp) lisp-code)
  (exec (exe-path lisp-impl)
        (list "-norc"
              "-m" "100MB"
              "-x" (code-str lisp-code))))

(defclass ccl (single-exe-lisp-impl) ())

(defmethod run-lisp-process ((lisp-impl ccl) lisp-code)
  (exec (exe-path lisp-impl)
        (list "--no-init"
              "--eval" (code-str lisp-code)
              "--eval" "(ccl:quit)")))

(defclass abcl (lisp-impl) 
  ((java-exe-path :type string
                  :accessor java-exe-path
                  :initarg :java-exe-path
                  :initform (error "java-exe-path must be specified"))
   (abcl-jar-path :type string
                  :accessor abcl-jar-path
                  :initarg :abcl-jar-path
                  :initform (error "abcl-jar-path must be specified"))))

(defmethod run-lisp-process ((lisp-impl abcl) lisp-code)
  (exec (java-exe-path lisp-impl)
        (list "-jar"
              (abcl-jar-path lisp-impl)
              "--noinit"
              "--nosystem"
              "--batch"
              "--eval" (code-str lisp-code))))

(defclass sbcl (single-exe-lisp-impl) ())

(defmethod run-lisp-process ((lisp-impl sbcl) lisp-code)
  (exec (exe-path lisp-impl)
        (list "--noinform"
              "--end-runtime-options"
              "--no-sysinit"
              "--no-userinit"
              "--eval" (code-str lisp-code)
              "--eval" "(sb-ext:quit)")))

(defclass ecl (single-exe-lisp-impl) ())
(defmethod run-lisp-process ((lisp-impl ecl) lisp-code)
  (exec (exe-path lisp-impl)
        (list "-norc"
              "-eval" (code-str lisp-code)
              "-eval" "(ext:quit)")))

(defclass acl (single-exe-lisp-impl) ())

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

(defmethod run-lisp-process ((lisp-impl acl) lisp-code)
  ;; docs: http://www.franz.com/support/documentation/6.2/doc/startup.htm#command-line-args-1
  (exec (exe-path lisp-impl)
        (list #+windows "+c" ;; no console window
              #+windows "+B" ;; no splash screen
              "-qq" ;; don't read any initialization files
              "-batch"
              "-ee" (escape-process-parameter-for-allegro
                     (code-str lisp-code))
              "-ee" "(excl:exit 0)")))

;; Lispwork is not tested as I don't have a license,
;; and the free personal edition doesn't have
;; command line version, only GUI.
(defclass lispworks (single-exe-lisp-impl) ())
(defmethod run-lisp-process ((lisp-impl lispworks) lisp-code)
  ;; docs: http://www.lispworks.com/documentation/lw60/LW/html/lw-484.htm
  (exec (exe-path lisp-impl)
        (list "-init" "-"     ;; don't read
              "-siteinit" "-" ;; any initialization files
              "-eval" (code-str lisp-code)
              "-eval" "(lispworks:quit)")))
