;;;; This file should be placed into homepage of the OS user running cl-test-grid agent.
;;;; When this file is loaded, current package is test-grid. 
;;;; The agent to be configured is available as test-grid:*agent*.

(defparameter *abcl* (make-instance 'abcl 
                                    :java-exe-path "java" 
                                    :abcl-jar-path "C:\\Users\\anton\\unpacked\\abcl\\abcl-bin-1.0.1\\abcl.jar"))
(defparameter *clisp* (make-instance 'clisp :exe-path "clisp"))
(defparameter *ccl-1.7-x86* (make-instance 'ccl :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.7-windows\\wx86cl.exe"))
(defparameter *ccl-1.7-x86-64* (make-instance 'ccl :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.7-windows\\wx86cl64.exe"))
(defparameter *ccl-1.8-x86* (make-instance 'ccl :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl.exe"))
(defparameter *ccl-1.8-x86-64* (make-instance 'ccl :exe-path "C:\\Users\\anton\\unpacked\\ccl\\ccl-1.8-windows\\wx86cl64.exe"))
(defparameter *sbcl* (make-instance 'sbcl :exe-path "sbcl"))
(defparameter *cmucl* (make-instance 'sbcl :exe-path "/opt/cmucl-20c/bin/lisp"))
(defparameter *ecl* (make-instance 'ecl :exe-path "C:\\Users\\anton\\projects\\ecl\\bin\\ecl.exe"))
(defparameter *ecl-old* (make-instance 'ecl :exe-path "C:\\Users\\anton\\unpacked\\ecl\\ecl-11.1.1\\bin\\ecl.exe"))
(defparameter *acl* (make-instance 'acl :exe-path "C:\\Program Files (x86)\\acl82express\\alisp.exe"))

(setf (lisps *agent*) (list *abcl* *clisp* *ccl-1.8-x86* *ccl-1.8-x86-64* *ccl-1.7-x86* *ccl-1.7-x86-64* *sbcl* *ecl* *ecl-old* *acl*)
      (preferred-lisp *agent*) *ccl-1.8-x86*

      ;; Please enter your email so that we know who is submitting the test results.
      ;; Also the email will be published in the online reports, and the library
      ;; authors can later contact you in case of questions about this test run,
      ;; your environment, etc.
      ;; 
      ;; If you are strongly opposed to publishing you email, please type e.g. 
      ;; just some nickname.
      (user-email *agent*) "avodonosov@yandex.ru")

