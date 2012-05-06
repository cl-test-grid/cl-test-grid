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

(defparameter *all-libs*
  '(:alexandria            :babel               :trivial-features    :cffi
    :cl-ppcre              :usocket             :flexi-streams       :bordeaux-threads
    :cl-base64             :cl-fad              :trivial-backtrace   :puri
    :anaphora              :parenscript         :trivial-garbage     :iterate
    :metabang-bind         :cl-json             :cl-containers       :metatilities-base
    :cl-cont               :moptilities         :trivial-timeout     :metatilities
    :named-readtables      :arnesi              :local-time          :s-xml
    :cl-oauth              :cl-routes           :cl-unicode          :fiveam
    :trivial-utf-8         :yason               :cl-annot            :cl-openid
    :split-sequence        :cl-closure-template :cl-interpol         :trivial-shell
    :let-plus              :data-sift           :cl-num-utils        :ieee-floats
    :cl-project            :trivial-http        :cl-store            :hu.dwim.stefil
    :kmrcl                 :cxml-stp            :hu.dwim.walker      :hu.dwim.defclass-star
    :bknr.datastore        :yaclml              :com.google.base    :external-program)
  "All the libraries currently supported by the test-grid.")

(defun clean-rt (&optional (rt-package :rtest))
  (require-impl "rt-api")
  (rt-api:clean rt-package))

(defun run-rt-test-suite(&optional (rt-package :rtest))
  (require-impl "rt-api")

  (let (non-compiled-failures all-failures)

    (rt-api:do-tests :compiled-p nil :rt-package rt-package)
    (setf non-compiled-failures (rt-api:failed-tests rt-package))

    (rt-api:do-tests :compiled-p t :rt-package rt-package)
    (setf all-failures (union non-compiled-failures
                              (rt-api:failed-tests rt-package)))

    (list :failed-tests (mapcar #'string-downcase all-failures)
          :known-to-fail (mapcar #'string-downcase (rt-api:known-to-fail rt-package)))))

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

(defun running-cl-test-more-suite (project-name runner-function)
  ;; cl-test-more test suites usually run tests the
  ;; load time.
  ;;
  ;; cl-test-more produces text output in
  ;; the TAP (Test Anhything Protocol),
  ;; and does not produce any other programmaticaly
  ;; inspectable value.
  ;;
  ;; We will intercept the test output, and
  ;; interpret it according to the TAP
  ;; format (we limit this by just
  ;; looking for strings starting with "not ok"
  ;; in the output).

  ;; Intersepting cl-test-more TAP output
  (quicklisp:quickload :cl-test-more)

  (let ((test-output-buf (make-string-output-stream)))
    (progv
        (list (read-from-string "cl-test-more:*test-result-output*"))
        (list (make-broadcast-stream *standard-output* test-output-buf))
      (funcall runner-function))

    ;; Now look for a strting starting from "not ok"
    ;; in the test output
    (with-input-from-string (test-output (get-output-stream-string test-output-buf))
      (do ((line (read-line test-output nil) (read-line test-output nil)))
          ((null line))
        (when (starts-with line "not ok")
          (format t "---------------------------------------------------------------------------~%")
          (format t "~A test suite has a test failure; the first TAP output failure string:~%~A"
                  project-name line)
          (return-from running-cl-test-more-suite :fail))))
    :ok))

(defun combine-extended-libresult (libresult-a libresult-b)
  (list :failed-tests (union (getf libresult-a :failed-tests)
                             (getf libresult-b :failed-tests)
                             :test #'string=)
        :known-to-fail (union (getf libresult-a :known-to-fail)
                              (getf libresult-b :known-to-fail)
                              :test #'string=)))

(assert (let ((combined (combine-extended-libresult '(:failed-tests ("a" "b")
                                                      :known-to-fail ("c"))
                                                    '(:failed-tests ("a2" "c2")
                                                      :known-to-fail ("b2")))))
          (and (set= '("a" "b" "a2" "c2")
                     (getf combined :failed-tests)
                     :test #'string=)
               (set= '("c" "b2")
                     (getf combined :known-to-fail)
                     :test #'string=))))

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
  (clean-rt #+sbcl :sb-rt #-sbcl :rtest)
  (asdf:clear-system :alexandria-tests)
  (quicklisp:quickload :alexandria-tests)

  (run-rt-test-suite #+sbcl :sb-rt #-sbcl :rtest))

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

  ;; Workaround for the quicklisp bug #55
  ;; (https://github.com/quicklisp/quicklisp-client/issues/55)
  (ql:quickload :cffi)
  (let ((cffi-dir (make-pathname :name nil :type nil :defaults (ql-dist:find-asdf-system-file "cffi"))))
    (pushnew cffi-dir asdf:*central-registry* :test #'equal))
  ;; now (ql:quickload "cffi-tests") will work

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
                            ;; absent. That's why use use ignore-errors
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

(defmethod libtest ((library-name (eql :cl-fad)))

  ;; The test framework used: cl:assert.

  (quicklisp:quickload :cl-fad)
  (load
   (asdf:system-relative-pathname (asdf:find-system :cl-fad)
                                  "test.lisp"))

  ;; cl-fad test suite uses cl:assert.
  ;; I.e. any test faifures are signaled as errors.
  (handler-case
      (progn
        (funcall (intern (symbol-name '#:test) '#:cl-fad-test))
        :ok)
    (error (e)
      (format t "cl-fad test suite failed with error: ~A" e)
      :fail)))

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
  (clean-rt #+sbcl :sb-rt #-sbcl :rtest)
  (asdf:clear-system :iterate-tests)
  (asdf:clear-system :iterate)

  ;; Load iterate first, because iterate-tests
  ;; is defined in iterate.asd which confuses
  ;; quicklisp (some versions of, see issue #1)
  (quicklisp:quickload :iterate)
  (quicklisp:quickload :iterate-tests)

  (run-rt-test-suite #+sbcl :sb-rt #-sbcl :rtest))

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

  ;; Instead of just doing
  ;;  (quicklisp:quickload :metatilities-base-test)
  ;; we need to workarount quicklisp issue
  ;; https://github.com/quicklisp/quicklisp-client/issues/58.
  (ql:quickload "metatilities-base")
  (let* ((lib-dir (make-pathname :name nil
                                 :type nil
                                 :defaults (ql-dist:find-asdf-system-file "metatilities-base")))
         (asdf:*central-registry* (cons lib-dir asdf:*central-registry*)))
    (ql:quickload "metatilities-base-test"))

  ;; now run the tests
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

(defmethod libtest ((library-name (eql :named-readtables)))
  ;; test framework used: customized RT
  (clean-rt :named-readtables-test)

  (asdf:clear-system :named-readtables)
  (asdf:clear-system :named-readtables-test)

  (quicklisp:quickload :named-readtables)
  (quicklisp:quickload :named-readtables-test)

  (run-rt-test-suite :named-readtables-test))

(defmethod libtest ((library-name (eql :arnesi)))
  ;; test framework used: FiveAM
  (quicklisp:quickload :arnesi)
  (quicklisp:quickload :arnesi.test)
  (run-fiveam-test-suite :it.bese.arnesi))

(defmethod libtest ((library-name (eql :local-time)))
  ;; test framework used: Stefil
  (quicklisp:quickload :local-time.test)
  (run-stefil-test-suite (intern (string '#:test) '#:local-time.test)))

(defmethod libtest ((library-name (eql :s-xml)))
  ;; test framework used: cl:assert

  (quicklisp:quickload :s-xml)

  ;; s-xml test suite uses cl:assert, and all
  ;; the assertsions are top level, i.e. executed
  ;; immediatelly during the system load
  (handler-case
      (progn
        (asdf:operate 'asdf:load-op :s-xml.test :force t)
        :ok)
    (error (e)
      (format t "s-xml test suite failed with error: ~A" e)
      :fail)))

(defun is-windows ()
  (intersection '(:windows :win32 :win) *features*))

;; See coverage.org for more info why IO lib is not added.
;;
;; (defmethod libtest ((library-name (eql :iolib)))
;;   ;; test framework used: FiveAM
;;
;;   (cond ((is-windows)
;;          (format t "IOLib is not implemented for Windows.~%")
;;          :no-resource)
;;         (t
;;          (quicklisp:quickload :iolib-tests)
;;          (run-fiveam-test-suite :iolib))))

(defmethod libtest ((library-name (eql :cl-oauth)))
  ;; test framework used: FiveAM

  (quicklisp:quickload :cl-oauth)
  (quicklisp:quickload :cl-oauth.tests)

  ;; the code is based on the method
  ;; (defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-oauth))))
  ;; from the cl-oauth sources

  (let ((original-request-adapter (symbol-value (read-from-string "cl-oauth:*request-adapter*"))))
    (unwind-protect
         (progn
           (funcall (read-from-string "oauth-test::init-test-request-adapter"))
           (run-fiveam-test-suite (read-from-string "oauth-test::oauth")))
      (setf (symbol-value (read-from-string "cl-oauth:*request-adapter*"))
            original-request-adapter))))

(defmethod libtest ((library-name (eql :cl-routes)))
  ;; The test framework used: lift.
  (quicklisp:quickload :routes)
  (quicklisp:quickload :routes-test)
                       ;; good way to refre symbols, thanks cl-routes
  (run-lift-test-suite (read-from-string "routes.test::routes-test")))

(defmethod libtest ((library-name (eql :cl-unicode)))
  ;; The test framework used: custom.
  (quicklisp:quickload :cl-unicode)
  (quicklisp:quickload :cl-unicode-test)
  (funcall (read-from-string "cl-unicode-test:run-all-tests")))

(defmethod libtest ((library-name (eql :fiveam)))
  ;; test framework used: FiveAM
  (quicklisp:quickload :fiveam)
  (run-fiveam-test-suite :it.bese.fiveam))

(defmethod libtest ((library-name (eql :trivial-utf-8)))
  ;; test framework used: cl:assert

  (quicklisp:quickload :trivial-utf-8)

  ;; trivial-utf-8-tests test suite uses cl:assert, and all
  ;; the assertsions are top level, i.e. executed
  ;; immediatelly during the system load
  (handler-case
      (progn
        (asdf:operate 'asdf:load-op :trivial-utf-8-tests :force t)
        :ok)
    (error (e)
      (format t "trivial-utf-8 test suite failed with error: ~A" e)
      :fail)))

(defmethod libtest ((library-name (eql :yason)))
  ;; test framework used: unit-test

  ;; Handle package name conflict between cl-json
  ;; and yason: the package of cl-json is named :json,
  ;; the package of yason is named :yason, but has
  ;; :json as a nickname.

  ;; Temporary rename the :json package if it's loaded
  (when (and (find-package :json)
             ;; make sure it's found not by a nickname
             (string= :json (package-name :json)))
    (rename-package :json :json-temp-uinuque-name))

  (quicklisp:quickload :yason)
  ;; it doesn't provide an ASDF system for tests,
  ;; load the test framework manually, and then
  ;; the test.lisp file.
  (quicklisp:quickload :unit-test)
  (let* ((yason-dir (make-pathname :name nil :type nil :defaults (ql-dist:find-asdf-system-file "yason")))
         (yason-test-file (merge-pathnames "test.lisp" yason-dir)))
    (format t "loading ~A~%" yason-test-file)
    (load yason-test-file))

  ;; remove nicknames from the :yason package
  (rename-package :yason :yason nil)

  ;; rename :json back to it's original name
  (when (find-package :json-temp-uinuque-name)
    (rename-package :json-temp-uinuque-name :json))

  ;; now run the tests. It returns a boolean
  (funcall (read-from-string "unit-test:run-all-tests") :unit :yason))

(defmethod libtest ((library-name (eql :cl-annot)))
  ;; test framework used: cl-test-more
  (running-cl-test-more-suite "cl-annot"
                              #'(lambda ()
                                  ;; ensure it is reloaded, even if it was already loaded before
                                  (asdf:clear-system :cl-annot)
                                  (asdf:clear-system :cl-annot-test)
                                  (quicklisp:quickload :cl-annot-test))))

(defmethod libtest ((library-name (eql :cl-openid)))
  ;; test framework used: FiveAM
  (ql:quickload :cl-openid)
  (ql:quickload :cl-openid.test)
  (run-fiveam-test-suite :cl-openid))

(defmethod libtest ((library-name (eql :split-sequence)))
  ;; test framework used: FiveAM
  (ql:quickload :split-sequence)
  (ql:quickload :split-sequence-tests)
  (run-fiveam-test-suite :split-sequence))

(defmethod libtest ((library-name (eql :cl-closure-template)))
  ;; The test framework used: lift.
  (ql:quickload :closure-template)
  (ql:quickload :closure-template-test)
  (run-lift-test-suite (read-from-string "closure-template.test::closure-template-test")))

(defmethod libtest ((library-name (eql :cl-interpol)))
  #+abcl
  (progn
    (format t "~&We do not run cl-interpol tests on ABCL, but just report a hardcoded failure,~%")
    (format t "because as of ABCL 1.0.1 cl-interpol crashes ABCL by ~%")
    (format t "\"java.lang.OutOfMemoryError: Java heap space\" and before that~%")
    (format t "produces megabytes of logs containing the error message:~%")
    (format t "   got an unexpected error: The value #<FLEXI-STREAMS:FLEXI-INPUT-STREAM {56E7A30E}> is not of type STREAM.~%")
    (format t "Increasing JVM heap size does not help - it just takes longer and produces more~%")
    (format t "logs, crashing finally with OutOfMemory anyway.~%")
    (return-from libtest :fail))

  ;; The test framework used: custom.
  (ql:quickload :cl-interpol)
  (ql:quickload :cl-interpol-test)
  (funcall (read-from-string "cl-interpol-test:run-all-tests")))

;; Decided not to add Lift now.
;; See coverage.org for details.
;;
;; (defmethod libtest ((library-name (eql :lift)))
;;   ;; The test framework used: lift.
;;   (ql:quickload :lift)
;;   (ql:quickload :lift-test)
;;   (run-lift-test-suite (read-from-string "lift-test::lift-test")))

(defmethod libtest ((library-name (eql :trivial-shell)))
  ;; The test framework used: lift.
  (cond ((is-windows)
         (format t "trivial-shell is not implemented for Windows.~%")
         :no-resource)
        (t
         (ql:quickload :trivial-shell)
         (ql:quickload :trivial-shell-test)
         (run-lift-test-suite (read-from-string "trivial-shell-test::trivial-shell-test")))))

(defmethod libtest ((library-name (eql :let-plus)))
  ;; The test framework used: lift.
  (ql:quickload :let-plus)
  (ql:quickload :let-plus-tests)
  (run-lift-test-suite (read-from-string "let-plus-tests::let-plus-tests")))

(defmethod libtest ((library-name (eql :data-sift)))
  ;; The test framework used: lift.
  (ql:quickload :data-sift)
  (ql:quickload :data-sift-test)
  (run-lift-test-suite (read-from-string "data-sift.test::data-sift-test")))

(defmethod libtest ((library-name (eql :cl-num-utils)))
  ;; The test framework used: lift.
  (ql:quickload :cl-num-utils)
  (ql:quickload :cl-num-utils-tests)
  (run-lift-test-suite (read-from-string "cl-num-utils-tests::cl-num-utils-tests")))

(defmethod libtest ((library-name (eql :ieee-floats)))
  ;; test framework used: FiveAM
  (ql:quickload :ieee-floats)
  (ql:quickload :ieee-floats-tests)
  (run-fiveam-test-suite :ieee-floats))

(defmethod libtest ((library-name (eql :cl-project)))
  ;; test framework used: cl-test-more
  (running-cl-test-more-suite "cl-project"
                              #'(lambda ()
                                  (ql:quickload :cl-project)
                                  (ql:quickload :cl-project-test))))

(defmethod libtest ((library-name (eql :trivial-http)))
  ;; The test framework used: lift.
  (ql:quickload :trivial-http)
  (ql:quickload :trivial-http-test)
  (run-lift-test-suite :trivial-http-test))

(defmethod libtest ((library-name (eql :cl-store)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :cl-store)
  (asdf:clear-system :cl-store-tests)

  (ql:quickload :cl-store-tests)

  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :hu.dwim.stefil)))
  ;; The test framework used: stefil.

  (ql:quickload :hu.dwim.stefil.test)
  (run-stefil-test-suite (read-from-string "hu.dwim.stefil.test::test")))

(defmethod libtest ((library-name (eql  :kmrcl)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :kmrcl-tests)

  (ql:quickload :kmrcl-tests)

  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :cxml-stp)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :cxml-stp)
  (asdf:clear-system :cxml-stp-test)

  (ql:quickload :cxml-stp)
  (ql:quickload :cxml-stp-test)

  (run-rt-test-suite))

(defmethod libtest ((library-name (eql :hu.dwim.walker)))
  ;; The test framework used: stefil.
  (ql:quickload :hu.dwim.walker.test)
  (run-stefil-test-suite (read-from-string "hu.dwim.walker.test::test")))

(defmethod libtest ((library-name (eql :hu.dwim.defclass-star)))
  ;; The test framework used: stefil.
  (ql:quickload :hu.dwim.defclass-star.test)
  (run-stefil-test-suite (read-from-string "hu.dwim.defclass-star.test::test")))

(defmethod libtest ((library-name (eql :bknr.datastore)))
  ;; test framework used: FiveAM
  (ql:quickload :bknr.datastore)
  (ql:quickload :bknr.datastore.test)
  (run-fiveam-test-suite :bknr.datastore))

(defmethod libtest ((library-name (eql :yaclml)))
  ;; test framework used: FiveAM

  ;; Handle package name conflict between s-xml and
  ;; yaclml. s-xml creates a package named :xml,
  ;; while yaclml uses :xml as a nickname for it's package.

  ;; Temporary rename the :xml package if it's loaded
  (when (and (find-package :xml)
             ;; make sure it's found not by a nickname
             (string= :xml (package-name :xml)))
    (rename-package :xml :xml-temp-uinuque-name))

  (ql:quickload :yaclml)

  ;; remove nicknames from the :yaclml package
  (rename-package :it.bese.yaclml.xml :it.bese.yaclml.xml nil)
  ;; rename :json back to it's original name
  (when (find-package :xml-temp-uinuque-name)
    (rename-package :xml-temp-uinuque-name :xml))

  (ql:quickload :yaclml.test)
  (run-fiveam-test-suite :it.bese.yaclml))

(defmethod libtest ((library-name (eql :com.google.base)))
  ;; The test framework used: stefil.
  (ql:quickload :com.google.base-test)
  (run-stefil-test-suite (read-from-string "com.google.base-test::test-base")))

(defmethod libtest ((library-name (eql :external-program)))
  ;; test framework used: FiveAM
  (when (is-windows)
    (format t "The external-program test suite uses unix shell commands, like cd, which, and therefor can not be tested on Windows.")
    (return-from libtest :no-resource))

  #+ecl
  (progn
    (format t "external-program test suite hangs on ECL (as of ecl-11.1.1-606449eb-linux-x86, Quicklisp 2012-03-07 (external-program-20111001-git)~%")
    (format t "Returning hardcoded :FAIL~%")
    (return-from libtest :fail))

  (ql:quickload :external-program)
  (ql:quickload :external-program-test)
  (run-fiveam-test-suite (read-from-string "external-program-tests::tests")))

;; see coverage.org for details why weblocks is not included into the test grid
;;
;; (defmethod libtest ((library-name (eql :weblocks)))
;;   ;; The test framework used: lift.
;;
;;   (when (find-package :yason)
;;     (warn "Deleting package :yason and ASDF system :yason because weblocks depends on cl-json and cl-json has a package name conflict with yason (cl-json main package name is :json, and yason has nickname :json")
;;     (delete-package :yason)
;;     (asdf:clear-system :yason))
;;
;;   (ql:quickload :weblocks-test)
;;   (ql:quickload :weblocks-store-test)
;;
;;   (combine-extended-libresult (run-lift-test-suite :weblocks-suite)
;;                               (run-lift-test-suite :store-suite)))

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

        (setf status
              ;; We should never allow the test suite to enter debugger.
              ;; If the test suite tries to enter debugger, then something
              ;; is wrong, we just record failure.
              (restart-case
                  (let ((*debugger-hook* #'(lambda (condition me-or-my-encapsulation)
                                             (declare (ignore me-or-my-encapsulation))
                                             (format t
                                                     "The test suite calls invoke-debugger with condition of type ~A: ~A.~%"
                                                     (type-of condition)
                                                     condition)
                                             (invoke-restart 'fail-the-test-suite-because-of-debugger))))
                    ;; Even despite we prevent entering the interactive debugger,
                    ;; we capture all the SERIOURS-CONDITIONS signalled by test suite,
                    ;; because we don't want our caller (the code calling RUN-LIBTEST),
                    ;; to see the test suite errors as errors signalled by RUN-LIBTEST.
                    (handler-case
                        (normalize-status (libtest lib))
                      (serious-condition (condition) (progn
                                                       (format t
                                                               "~&Unhandled SERIOUS-CONDIDION of type ~A is signaled: ~A~%"
                                                               (type-of condition)
                                                               condition)
                                                       :fail))))
                (fail-the-test-suite-because-of-debugger ()
                  (format t "The test suite stopped because of the debugger invocation. Returning :FAIL.")
                  :fail)))

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

