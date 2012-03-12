;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: test-grid; Base: 10; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

(defpackage #:test-grid (:use :cl))

(in-package #:test-grid)

(defgeneric libtest (library-name)
  (:documentation "Define a method for this function
with LIBRARY-NAME eql-specialized for for every library added
to the test grid.

The method should run test suite and return the resulting
status. Status is one of these values:
  :OK - all tests passed,
  :FAIL - some test failed,
  :NO-RESOURCE - test suite can not be run because some required
     resource is absent in the environment. For example, CFFI library
     test suite needs a small C library compiled to DLL. User must
     do it manually. In case the DLL is absent, the LIBTEST method
     for CFFI returns :NO-RESOURCE.
  Extended status in the form
     (:FAILED-TESTS <list of failed tests> :KNOWN-TO-FAIL <list of known failures>)
     The test names in these lists are represented as downcased strings.

If any SERIOUS-CONDITION is signalled, this is considered a failure.

For convenience, T may be returned instead of :OK and NIL instead of :FAIL."))

(defun normalize-status (status)
  "Normilzies test resul status - converts T to :OK and NIL to :FAIL."
  (case status
    ((t :ok) :ok)
    ((nil :fail) :fail)
    (otherwise status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My Require
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun require-impl (api)
  "Loads an implementation of the specified API (if
it is not loaded yet).

Some of test-grid components have separate package
and ASDF system for API and separate package+ASDF
system for the implementation. This allows test-grid
to be compilable and (partially) opereable even
when some components are broken on particular lisp.

For these known test-grid componetns REQUIRE-IMPL loads
the implementation. Otherwise the API parameter is
just passed to the QUICKLISP:QUICKLOAD."
  (setf api (string-downcase api))
  (let* ((known-impls '(("rt-api" . "rt-api-impl")
                        ("lift-api" . "lift-api-impl")
                        ("fiveam-api" . "fiveam-api-impl")
                        ("eos-api" . "eos-api-impl")
                        ("stefil-api" . "stefil-api-impl")))
         (impl-asdf-system (or (cdr (assoc api known-impls :test #'string=))
                               api)))
        (quicklisp:quickload impl-asdf-system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIBTEST implementations for particular libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *all-libs* '(:alexandria :babel :trivial-features :cffi
                           :cl-ppcre :usocket :flexi-streams :bordeaux-threads
                           :cl-base64 :trivial-backtrace :puri :anaphora
                           :parenscript :trivial-garbage :iterate :metabang-bind
                           :cl-json :cl-containers :metatilities-base :cl-cont
                           :moptilities :trivial-timeout :metatilities)
  "All the libraries currently supported by the test-grid.")


(defun clean-rt ()
  (require-impl "rt-api")
  (rt-api:clean))

(defun run-rt-test-suite()
  (require-impl "rt-api")

  (let (non-compiled-failures all-failures)

    (rt-api:do-tests :compiled-p nil)
    (setf non-compiled-failures (rt-api:failed-tests))

    (rt-api:do-tests :compiled-p t)
    (setf all-failures (union non-compiled-failures
                              (rt-api:failed-tests)))

    (list :failed-tests (mapcar #'string-downcase all-failures)
          :known-to-fail (mapcar #'string-downcase (rt-api:known-to-fail)))))

(defun run-lift-test-suite (test-suite-name)
  (require-impl "lift-api")
  (let ((result (lift-api:run-test-suite test-suite-name)))
    (list :failed-tests (lift-api:failed-tests result)
          :known-to-fail (lift-api:known-to-fail test-suite-name))))

(defun run-fiveam-test-suite (test-suite-spec)
  (require-impl "fiveam-api")
  (let ((result (fiveam-api:run-test-suite test-suite-spec)))
    (list :failed-tests (fiveam-api:failed-tests result)
          :known-to-fail '())))

(defun run-eos-test-suites (&rest test-suite-specs)
  (require-impl "eos-api")
  (let ((result (apply #'eos-api:run-test-suites test-suite-specs)))
    (list :failed-tests (eos-api:failed-tests result)
          :known-to-fail '())))

(defun run-stefil-test-suite (test-suite-spec)
  (require-impl "stefil-api")
  (let ((result (stefil-api:run-test-suite test-suite-spec)))
    (list :failed-tests (stefil-api:failed-tests result)
          :known-to-fail '())))

(defmethod libtest ((library-name (eql :alexandria)))

; We keep the below hardcoded failure in case we want to test
; the ECL 11.1.1 release (until new release is out).
; It is commented out or uncommented depending on what
; ECL release we are going to test.

  ;; #+ecl
  ;; (progn
  ;;   (format t "ECL 11.1.1 has bug causing a stack overflow on alexandria tests. http://sourceforge.net/tracker/?func=detail&aid=3463131&group_id=30035&atid=398053~%")
  ;;   (return-from libtest :fail))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :alexandria-tests)
  (quicklisp:quickload :alexandria-tests)

  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :babel)))

  ;; The test framework used: stefil.

  (quicklisp:quickload :babel-tests)
  (run-stefil-test-suite (intern (string '#:babel-tests) '#:babel-tests)))

(defmethod libtest ((library-name (eql :trivial-features)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :trivial-features-tests)

  ;; Load cffi-grovel which is used in trivial-features-tests.asd,
  ;; but not in the asdf:defsystem macro (issue #2).
  (quicklisp:quickload :cffi-grovel)
  (quicklisp:quickload :trivial-features-tests)

  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :cffi)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :cffi-tests)

  ;; CFFI tests work with a small test C library.
  ;; The user is expected to compile the library manually.
  ;; If the library is not available, CFFI tests
  ;; signal cffi:load-foreign-library-error.
  (handler-bind ((error #'(lambda (condition)
                            ;; Check if the error is a
                            ;; cffi:load-foreign-library-error.
                            ;; Take into account that it might
                            ;; be some other error which prevents
                            ;; even CFFI system to load,
                            ;; and therefore CFFI package may be
                            ;; abset. That's why use use ignore-errors
                            ;; when looking for a symbol in the CFFI
                            ;; packge.
                            (when (eq (type-of condition)
                                      (ignore-errors (find-symbol (symbol-name '#:load-foreign-library-error)
                                                                  '#:cffi)))
                              ;; todo: add the full path to the 'test' directory,
                              ;; where user can find the scripts to compile
                              ;; the test C library, to the error message.
                              (format t
                                      "~& An error occurred during (quicklisp:quickload :cffi-tests):~%~A~&This means the small C library used by CFFI tests is not available (probably you haven't compiled it). The compilation script for the library may be found in the 'test' directory in the CFFI distribution.~%"
                                      condition)
                              (return-from libtest :no-resource))
                            ;; resignal the condition
                            (error condition))))
    (quicklisp:quickload :cffi-tests))
  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :cl-ppcre)))

  #+abcl
  (progn
    (format t "~&Due to ABCL bug #188 (http://trac.common-lisp.net/armedbear/ticket/188)~%")
    (format t "cl-ppcre tests fail, repeating this error huge number of times, in result~%")
    (format t "hanging for a long time and producing a huge log.")
    (return-from libtest :fail))

  ;; The test framework used: custom.

  ;; Workaround the quicklisp issue #225 -
  ;; https://github.com/quicklisp/quicklisp-projects/issues/225 -
  ;; first load cl-ppcre-unicode, because otherwise
  ;; current quicklisp can not find cl-ppcre-unicode-test
  (quicklisp:quickload :cl-ppcre-unicode)
  (quicklisp:quickload :cl-ppcre-unicode-test)

  ;; copy/paste from cl-ppcre-unicode.asd
  (funcall (intern (symbol-name :run-all-tests) (find-package :cl-ppcre-test))
           :more-tests (intern (symbol-name :unicode-test) (find-package :cl-ppcre-test))))

(defmethod libtest ((library-name (eql :usocket)))

  #+abcl
  (progn
    (format t "~&On ABCL abcl-1.0.0 the usocket test suite hangs (after producing significant~%")
    (format t "number of errors/failures in the log). The last log message before it hangs is:~%")
    (format t "USOCKET-TEST::WAIT-FOR-INPUT.3")
    (return-from libtest :fail))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :usocket-test)

;  (asdf:operate 'asdf:load-op :usocket-test :force t)

  (quicklisp:quickload :usocket-test)

  ;; TODO: usocket test suite might need manual configuration,
  ;;       see their README. Distinguish the case
  ;;       when the manual configuration hasn't been
  ;;       performed and return :no-resource status.
  ;;
  ;; (setf usocket-test::*common-lisp-net*
  ;;       (or usocket-test::*common-lisp-net*
  ;;           "74.115.254.14"))

  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :flexi-streams)))

  ;; The test framework used: custom.

  (quicklisp:quickload :flexi-streams-test)

  ;; copy/paste from flexi-streams.asd
  (funcall (intern (symbol-name :run-all-tests)
                   (find-package :flexi-streams-test))))

(defmethod libtest ((library-name (eql :bordeaux-threads)))

  #+cmucl
  (progn
    (format t "~&On CMUCL bordeaux-threads test suite traps into some active~%")
    (format t "deadlock, produces 8 MB of '.' symbols in log, constantly runs GC~%")
    (format t "and finally dies when heap is exhausted.~%")
    (return-from libtest :fail))

  ;; The test framework used: fiveam.

  (quicklisp:quickload :bordeaux-threads-test)

  (run-fiveam-test-suite :bordeaux-threads))

(defmethod libtest ((library-name (eql :cl-base64)))

  ;; The test framework used: ptester.

  ;; Load cl-base64 first, because cl-base64-tests
  ;; is defined in cl-base64.asd which confuses
  ;; quicklisp (some versions of, see issue #1)
  (quicklisp:quickload :cl-base64)

  (quicklisp:quickload :cl-base64-tests)

  (funcall (intern (symbol-name '#:do-tests)
                   (find-package '#:cl-base64-tests))))

(defmethod libtest ((library-name (eql :trivial-backtrace)))
  ;; The test framework used: lift.
  (quicklisp:quickload :trivial-backtrace-test)
  (run-lift-test-suite :trivial-backtrace-test))

(defmethod libtest ((library-name (eql :puri)))

  ;; The test framework used: ptester.

  ;; Load puri first, because puri-tests
  ;; is defined in puri.asd which confuses
  ;; quicklisp (some versions of, see issue #1)
  (quicklisp:quickload :puri)

  (quicklisp:quickload :puri-tests)

  ;; copy/paste from puri.asd
  (funcall (intern (symbol-name '#:do-tests)
                   (find-package :puri-tests))))

(defmethod libtest ((library-name (eql :anaphora)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :anaphora-test)
  ;; anaphora-test is defined in anaphora.asd,
  ;; therefore to reload :anaphora-test
  ;; we need to clean the :anaphora system too
  (asdf:clear-system :anaphora)

  ;; Load anaphora first, because anaphora-tests
  ;; is defined in anaphora.asd which confuses
  ;; quicklisp (some versions of, see issue #1)
  (quicklisp:quickload :anaphora)
  (quicklisp:quickload :anaphora-test)

  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :parenscript)))
  ;; The test framework used: eos (similar to FiveAM).

  (quicklisp:quickload :parenscript.test)

  ;; asdf:test-op is not provided for parenscript,
  ;; only a separate package ps-test with public
  ;; function run-tests.

  ;; The test suites to run are taken from the
  ;; the function run-tests in the file
  ;; <parenscript sources>/t/test.lisp.
  (run-eos-test-suites (intern (string '#:output-tests) :ps-test)
                       (intern (string '#:package-system-tests) :ps-test)
                       (intern (string '#:eval-tests) :ps-test)))

(defmethod libtest ((library-name (eql :trivial-garbage)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :trivial-garbage)  ; yes, trivial-garbage but not trivial-garbage-tests,
                                        ; because the trivial-garbage-tests system is defined
                                        ; in the same trivial-garbage.asd and neither
                                        ; asdf nor quicklisp can't find trivial-garbage-tests.

  (quicklisp:quickload :trivial-garbage); trivial-garbage but not trivial-garbage-tests,
                                        ; for the same reasons as explained above.
  (asdf:operate 'asdf:load-op :trivial-garbage-tests)

  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :iterate)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :iterate-tests)
  (asdf:clear-system :iterate)

  ;; Load iterate first, because iterate-tests
  ;; is defined in iterate.asd which confuses
  ;; quicklisp (some versions of, see issue #1)
  (quicklisp:quickload :iterate)
  (quicklisp:quickload :iterate-tests)

  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :metabang-bind)))

  ;; The test framework used: lift.

  ;; metabang-bind-test includes binding syntax
  ;; for regular-expression and corresponding
  ;; tests; but this functionality is only
  ;; loaded if cl-ppcre is loaded first.
  ;; (this conditional loading is achieaved
  ;; with asdf-system-connections).
  (quicklisp:quickload :cl-ppcre)
  (quicklisp:quickload :metabang-bind-test)

  (run-lift-test-suite :metabang-bind-test))

(defmethod libtest ((library-name (eql :cl-json)))
  ;; The test framework used: fiveam.
  (let ((*trace-output* *standard-output*))

    ;; Load cl-json first, because cl-json.tests
    ;; is defined in cl-json.asd which confuses
    ;; quicklisp (some versions of, see issue #1)
    (quicklisp:quickload :cl-json)

    (quicklisp:quickload :cl-json.test)
    (run-fiveam-test-suite (intern (symbol-name '#:json) :json-test))))

(defmethod libtest ((library-name (eql :cl-containers)))
  ;; The test framework used: lift.
  (quicklisp:quickload :cl-containers-test)
  (run-lift-test-suite :cl-containers-test))

(defmethod libtest ((library-name (eql :metatilities-base)))
  ;; The test framework used: lift.
  (quicklisp:quickload :metatilities-base-test)
  (run-lift-test-suite :metatilities-base-test))

(defmethod libtest ((library-name (eql :cl-cont)))
  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :cl-cont-test)
  (quicklisp:quickload :cl-cont-test)
  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :moptilities)))
  ;; The test framework used: lift.
  (quicklisp:quickload :moptilities-test)
  (run-lift-test-suite :moptilities-test))

(defmethod libtest ((library-name (eql :trivial-timeout)))
  ;; The test framework used: lift.
  (quicklisp:quickload :trivial-timeout-test)
  (run-lift-test-suite :trivial-timeout-test))

(defmethod libtest ((library-name (eql :metatilities)))
  ;; The test framework used: lift.
  (quicklisp:quickload :metatilities-test)
  (run-lift-test-suite :metatilities-test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set= (set-a set-b &key (test #'eql) key)
  (null (set-exclusive-or set-a set-b :test test :key key)))

(defun do-plist-impl (plist handler)
  (do* ((cur-pos plist (cddr cur-pos))
        (prop (first cur-pos) (first cur-pos))
        (val (second cur-pos) (second cur-pos)))
       ((null prop))
    (funcall handler prop val)))

(defmacro do-plist ((key val plist &optional result) &body body)
  `(block nil
     (do-plist-impl ,plist (lambda (,key ,val) ,@body))
     ,result))

(defun plist-comparator (&rest props-and-preds)
  (lambda (plist-a plist-b)
    (do-plist (prop pred props-and-preds)
      ;; iterate over all the property/predicate pairs
      ;; "compare" the values of the current property
      ;; in both plists
      (let ((val-a (getf plist-a prop))
            (val-b (getf plist-b prop)))
        (if (funcall pred val-a val-b)
            (return t))
        ;; Ok, val-a is not less than val-b (as defined by our predicate).
        ;; Lets check if they are equal. If the reverse comparation [val-b less val-a]
        ;; is also false, then they are equal, and we proceed to the next
        ;; property/predicate pair.
        (when (funcall pred val-b val-a)
          (return nil))))))

;; examples:
#|
 (let ((less (plist-comparator :a '< :b 'string<)))
   (and (funcall less '(:a 1 :b "x") '(:a 2 :b "y"))
        (funcall less '(:a 2 :b "x") '(:a 2 :b "y"))
        (not (funcall less '(:a 3 :b "x") '(:a 2 :b "y")))))

 (equalp
  (sort '((:a 1 :b "x")
          (:a 2 :b "y")
          (:a 2 :b "y")
          (:a 3 :b "z"))
        (plist-comparator :a '< :b 'string<))
  '((:A 1 :B "x") (:A 2 :B "y") (:A 2 :B "y") (:A 3 :B "z")))
|#

(defun getter (prop)
  #'(lambda (plist)
      (getf plist prop)))

(defun list< (predicates l1 l2)
  "Compares two lists L1 and L2 of equal lenght,
using for every pair of elements a corresponding predicate
from the PREDICATES list (of the same length). Returns
T if L1 is less than (according the PREDICATES) L2.
Othersise returns NIL."
  (if (null predicates)
      nil
      (let ((pred (car predicates))
            (elem1 (car l1))
            (elem2 (car l2)))
        (if (funcall pred elem1 elem2)
            t
            ;; Ok, elem1 is not less than elem2 (as defined by our predicate).
            ;; Lets check if they are equal. If the reverse comparation [elem2 less elem1]
            ;; is also false, then they are equal, and we proceed to the next
            ;; property/predicate pair.
            (if (funcall pred elem2 elem1)
                nil
                (list< (cdr predicates)
                       (cdr l1)
                       (cdr l2)))))))

#|
Examples:

 (and
  (list< '(< <) '(1 2) '(2 2))
  (not (list< '(< <) '(1 2) '(1 2)))
  (list< '(< <) '(1 2) '(1 3))
  (not (list< '(string< string<)
              '("quicklisp-fake-2011-00-02" "ccl-fake-1")
              '("quicklisp-fake-2011-00-01" "clisp-fake-1"))))
|#

(defun hash-table-keys (hash-table)
  (let (keys)
    (maphash #'(lambda (key val)
                 (declare (ignore val))
                 (push key keys))
             hash-table)
    keys))

;; copy/paste from
;; http://www.gigamonkeys.com/book/practical-an-mp3-browser.html
(defmacro with-safe-io-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*read-eval* nil))
       ,@body)))

(defun safe-read (&rest args)
  (with-safe-io-syntax (apply #'read args)))

(defun safe-read-file (file)
  (with-open-file (in file
                      :direction :input
                      :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                      )
    (safe-read in)))

(defun write-to-file (obj file)
  "Write to file the lisp object OBJ in a format acceptable to READ."
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (pprint obj out))
  obj)

;; based on
;; http://cl-user.net/asp/-1MB/sdataQ0mpnsnLt7msDQ3YNypX8yBX8yBXnMq=/sdataQu3F$sSHnB==
;; but fixed in respect to file-length returing file length in bytes
;; instead of characters (and violating the spec therefore) at least
;; on CLISP 2.49 and ABCL 1.0.0.
(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len))
           (char-len (read-sequence data s)))
      (if (> len char-len)
          (setf data (subseq data 0 char-len)))
      data)))

(defun file-byte-length (path)
  (with-open-file (s path
                     :direction :input
                     :element-type '(unsigned-byte 8))
    (file-length s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +settings-file-name+ "cl-test-grid-settings.lisp")

(defun get-settings-file()
  (merge-pathnames (user-homedir-pathname) +settings-file-name+))

(defun prompt-for-email ()
  (format *query-io* "~&~%")
  (format *query-io* "Please enter your email so that we know who is submitting the test results.~%")
  (format *query-io* "Also the email will be published in the online reports, and the library~%")
  (format *query-io* "authors can later contact you in case of questions about this test run, ~%")
  (format *query-io* "your environment, etc.~%~%")

  (format *query-io* "If you are strongly opposed to publishing you email, please type e.g. some nickname or just \"none\".~%~%")

  (format *query-io* "The value you enter will be saved and reused in the future. You can change~%")
  (format *query-io* "it in the file ~A in your home directory.~%~%" +settings-file-name+)

  (format *query-io* "email: ")

  (force-output *query-io*)
  (string-trim " " (read-line *query-io*)))

(defun get-user-email ()
  (let ((user-email nil))
    (handler-case
        (progn
          (setf user-email (getf (safe-read-file (get-settings-file))
                                 :user-email))
          (if (zerop (length user-email))
              (warn "Empty email is specified in the settings file ~a~%" (get-settings-file))))
      (file-error ()
        (progn
          (setf user-email (prompt-for-email))
          (write-to-file (list :user-email user-email)
                         (get-settings-file)))))
    user-email))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-descr (run)
  "The description part of the test run."
  (getf run :descr))

(defun run-results (run)
  "The list of test suite statuses for every library in the specified test run."
  (getf run :results))

(defun (setf run-results) (new-run-results test-run)
  (setf (getf test-run :results) new-run-results))

(defun make-run (description lib-results)
  (list :descr description :results lib-results))

(defun fmt-time (universal-time &optional destination)
  "The preferred time format used in the cl-test-grid project."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time 0)
    (funcall #'format
             destination
             "~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
             year month date hour min sec)))

(defun pretty-fmt-time (universal-time &optional destination)
  "The human-readable time format, used in reports."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time 0)
    (funcall #'format
             destination
             "~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
             year month date hour min sec)))

(defun make-run-descr ()
  "Generate a description for a test run which might be
performed in the current lisp system."
  (list :lisp (asdf::implementation-identifier)
        :lib-world (format nil "quicklisp ~A"
                           (ql-dist:version (ql-dist:dist "quicklisp")))
        :time (get-universal-time)
        :run-duration :unknown
        :contact (list :email (get-user-email))))

(defun name-run-directory (run-descr)
  "Generate name for the directory where test run
data (libraries test suites output and the run results) will be saved."
  (format nil
          "~A-~A"
          (fmt-time (getf run-descr :time))
          (getf run-descr :lisp)))

(defun test-output-base-dir ()
  (merge-pathnames "test-runs/"
                   test-grid-config:*src-base-dir*))

(defun run-directory (run-descr)
  (merge-pathnames (make-pathname
                    :directory (list :relative (name-run-directory run-descr))
                    :name      nil
                    :type      nil)
                   (test-output-base-dir)))

(defun lib-log-file (test-run-directory lib-name)
  (merge-pathnames (string-downcase lib-name)
                   test-run-directory))

(defun print-log-header (libname run-descr stream)
  (let ((*print-case* :downcase) (*print-pretty* nil))
    (format stream "============================================================~%")
    (format stream "  cl-test-grid test run~%")
    (format stream "------------------------------------------------------------~%")
    (format stream "  library:           ~A~%" libname)
    (format stream "  lib-world:         ~A~%" (getf run-descr :lib-world))
    (format stream "  lisp:              ~A~%" (getf run-descr :lisp))
    (format stream "  *features*:        ~A~%" (sort (copy-list *features*) #'string<))
    (format stream "  contributor email: ~A~%" (getf (getf run-descr :contact) :email))
    (format stream "  timestamp:         ~A~%" (pretty-fmt-time (get-universal-time)))
    (format stream "============================================================~%~%")))

(defun print-log-footer (libname status stream)
  (let ((*print-case* :downcase))
    (fresh-line stream)
    (terpri stream)
    (format stream "============================================================~%")
    (format stream "  cl-test-grid status for ~A: ~A~%"
            libname (print-test-status nil status))
    (format stream "============================================================~%")))

(defun run-libtest (lib run-descr log-directory)
  (let (status
        (log-file (lib-log-file log-directory lib))
        (start-time (get-internal-real-time)))
    (with-open-file (log-stream log-file
                                :direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create)
      (let* ((orig-std-out *standard-output*)
             (*standard-output* log-stream)
             (*error-output* log-stream))

        (format orig-std-out
                "Running tests for ~A. *STANDARD-OUTPUT* and *ERROR-OUTPUT* are redirected.~%"
                lib)
        (finish-output orig-std-out)

        (print-log-header lib run-descr *standard-output*)

        (setf status (handler-case
                         (normalize-status (libtest lib))
                       (serious-condition (condition) (progn
                                                        (format t
                                                                "~&Unhandled SERIOUS-CONDITION is signaled: ~A~%"
                                                                condition)
                                                        :fail))))
        (print-log-footer lib status *standard-output*)))

    (list :libname lib
          :status status
          :log-byte-length (file-byte-length log-file)
          :test-duration (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second))))

(defun run-info-file (test-run-directory)
  (merge-pathnames "test-run-info.lisp"
                   test-run-directory))

(defun save-run-info (test-run directory)
  (let ((run-file (run-info-file directory)))
    (with-open-file (out run-file
                         :direction :output
                         :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (print-test-run out test-run))))

(defun gae-blobstore-dir ()
  (merge-pathnames "gae-blobstore/lisp-client/" test-grid-config:*src-base-dir*))

(defparameter *gae-blobstore-base-url* "http://cl-test-grid.appspot.com")

(defun get-blobstore ()
  (pushnew (truename (gae-blobstore-dir)) asdf:*central-registry* :test #'equal)
  (ql:quickload '#:test-grid-gae-blobstore)
  (funcall (intern (string '#:make-blob-store) '#:test-grid-gae-blobstore)
           :base-url *gae-blobstore-base-url*))

(defun submit-logs (blobstore test-run-dir)
  (let* ((run-info (safe-read-file (run-info-file test-run-dir)))
         ;; prepare parameters for the SUBMIT-FILES blobstore function
         (submit-params (mapcar #'(lambda (lib-result)
                                    (let ((libname (getf lib-result :libname)))
                                      (cons libname
                                            (lib-log-file test-run-dir libname))))
                                (run-results run-info))))
    ;; submit files to the blobstore and receive
    ;; their blobkeys in response
    (let ((libname-to-blobkey-alist
           (test-grid-blobstore:submit-files blobstore
                                             submit-params)))
      ;; Now store the blobkeys for every library in the run-info.
      ;; Note, we destructively modify parts of the previously
      ;; read run-info.
      (flet ((get-blob-key (lib)
               (or (cdr (assoc lib libname-to-blobkey-alist))
                   (error "blobstore didn't returned blob key for the log of the ~A libary" lib))))
        (setf (run-results run-info)
              (mapcar #'(lambda (lib-result)
                          (setf (getf lib-result :log-blob-key)
                                (get-blob-key (getf lib-result :libname)))
                          lib-result)
                      (run-results run-info))))
      ;; finally, save the updated run-info with blobkeys
      ;; to the file. Returns the run-info.
      (save-run-info run-info test-run-dir)
      run-info)))

(defun submit-results (test-run-dir)
  (let* ((blobstore (get-blobstore))
         (run-info (submit-logs blobstore test-run-dir)))
    (format t "The log files are submitted. Submitting the test run info...~%")
    (test-grid-blobstore:submit-run-info blobstore run-info)
    (format t "Done. The test results are submitted. They will be reviewed by admin soon and added to the central database.~%")
    run-info))

(defun run-libtests (&optional (libs *all-libs*))
  (let* ((run-descr (make-run-descr))
         (run-dir (run-directory run-descr))
         (lib-results))
    (ensure-directories-exist run-dir)
    (dolist (lib libs)
      (let ((lib-result (run-libtest lib run-descr run-dir)))
        (push lib-result lib-results)))
    (setf (getf run-descr :run-duration)
          (- (get-universal-time)
             (getf run-descr :time)))
    (let ((run (make-run run-descr lib-results)))
      (save-run-info run run-dir)
      (format t "The test results were saved to this directory: ~%~A.~%"
              (truename run-dir))
      run-dir)))

(defun submit-test-run (test-run-dir)
  (format t "~%Submitting the test results to the server...~%")
  (handler-case (submit-results test-run-dir)
    (error (e) (format t "Error occured while uploading the test results to the server: ~A: ~A.
Please submit manually the full content of the results directory
   ~A
to the cl-test-grid issue tracker:
   https://github.com/cl-test-grid/cl-test-grid/issues~%"
                       (type-of e)
                       e
                       (truename test-run-dir))))
  (format t "~%Thank you for the participation!~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *db* '(:version 0 :runs ()))

(defvar *standard-db-file*
  (merge-pathnames "db.lisp"
                   test-grid-config:*src-base-dir*))

(defun add-run (run-info &optional (db *db*))
  (push run-info (getf db :runs)))

(defun print-list-elements (destination list separator elem-printer)
  (let ((maybe-separator ""))
    (dolist (elem list)
      (format destination maybe-separator)
      (funcall elem-printer elem)
      (setf maybe-separator separator))))

(defun print-list (destination list separator elem-printer)
  (format destination "(")
  (print-list-elements destination list separator elem-printer)
  (format destination ")"))

(defun print-test-status (destination status)
  (etypecase status
    (symbol (format destination "~s" status))
    (list (progn
            (let ((dest (or destination (make-string-output-stream))))
              (flet ((test-name-printer (test-name)
                       (format dest "~s" test-name)))
                (format dest "(:failed-tests ")
                (print-list dest (sort (copy-list (getf status :failed-tests))
                                       #'string<)
                            " " #'test-name-printer)
                (format dest " :known-to-fail ")
                (print-list dest (sort (copy-list (getf status :known-to-fail))
                                              #'string<)
                            " " #'test-name-printer)
                (format dest ")"))
              (if (null destination)
                  (get-output-stream-string dest)
                  nil))))))

(defun print-test-run (out test-run &optional (indent 0))
  (let ((descr (getf test-run :descr)))
    (format out
            "(:descr (:lisp ~s :lib-world ~s :time ~s :run-duration ~s :contact (:email ~s))~%"
            (getf descr :lisp)
            (getf descr :lib-world)
            (getf descr :time)
            (getf descr :run-duration)
            (getf (getf descr :contact) :email)))
  (format out "~v,0t:results (" (1+ indent))
  (print-list-elements out
                       (sort (copy-list (getf test-run :results))
                             #'string<
                             :key #'(lambda (lib-result)
                                      (getf lib-result :libname)))
                       (format nil "~~%~~~Dt" (+ indent 11))
                       #'(lambda (lib-result)
                           (format out
                                   "(:libname ~s :status ~a :test-duration ~s :log-byte-length ~s :log-blob-key ~s)"
                                   (getf lib-result :libname)
                                   (print-test-status nil (getf lib-result :status))
                                   (getf lib-result :test-duration)
                                   (getf lib-result :log-byte-length)
                                   (getf lib-result :log-blob-key))))
  (format out "))"))

(defun save-db (&optional (db *db*) (stream-or-path *standard-db-file*))
  (with-open-file (out stream-or-path
                       :direction :output
                       :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "(:version ~a~%" (getf db :version))
    (format out " :runs (")
    (print-list-elements out
                         (getf db :runs)
                         "~%~8t"
                         #'(lambda (test-run)
                             (print-test-run out test-run 8)))
    (format out "))")))

(defun read-db (&optional (stream-or-path *standard-db-file*))
  (with-open-file (in stream-or-path
                      :direction :input
                      :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                      )
    (safe-read in)))

