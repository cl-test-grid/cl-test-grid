(defpackage #:test-grid (:use :cl))

(in-package #:test-grid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

TODO: 
  - information about test run: 
   + lisp-version-string, 
   + lib-world, 
   + author contact (get it from some settings file), 
   + date, 
   + run-duration
 + organize database file format
 + better decision for library name representation.
     This representation is used in the:
     - libtest method parameter (eql specialized)
     - in the database and testrun datastructure.
     Possible alternatives:
     a keyword
        good for READ (package independent),
        good for EQL specializer
        good for GETF when working with the database
     a symbol from the test-grid package
        - good for EQL specializer
        - package dependent in READ
        - good for GETF when working with the database
        - adds one more (unnecessary) way to represent a library 
          in addition the specified for ASDF and Quicklisp
     or a downcased string
        - needs special handling in libtest eql specialization
        - good ro READ (package independent)
        - needs care to work with GETF when working with the database      
 + Generate a sufficiently large database so that we can
   evaluate our reporting solution. We may generate
   fake test results programmatically to make this task
   easier.
 - simpliest reporting to allow overview of library test statuses 
   - Test Runs report: every test run as a row in a table
     + legend or a tooltip in the report for test statuses
     + color for statuses
     + use the online blob URL in the report
   - A pivot -like table report of library test results, allowing
     rows/columns to be any of quicklisp distro, lisp version
     library name. With grouping and sorging.
   - CSV export of the database to use it then with spreadsheets,
     google fusion tables, etc. Initial intent
     was to format it as a pivot for various projections 
     (by quicklisp releases, by platform, etc).
     But neither google docs spreadsheet, nor google fusion
     table allow as to format results as we want
     (the main problem, it is impossible to use
     a custom aggregation function for pivot
     cells, because standard aggregation functions
     are numeric, but we want a report cell
     to represent test result(s) for a particular
     library, i.e. :ok, :fail, :no-resource).
     5h
     - Test that the test-duration field value
       (Common Lisp rational) can be read
       by spreadsheet software (MS/Open Offices,
       Google Spreadsheets).
   - an informer which may be embedded into a library
     project page, with reports about the test statuses 
     for this single library on various platforms with
     various quicklisp versions
   - an overview page with brief explanation of all
     the above reports and links to the reports.
     - change 
       "represents every test run as a separate row" 
       to
       "represents every <tt>test-grid:run-tests</tt> as a separate row"
       (after user will know this command from the main project description)
       ?
     - Description of CSV report may link to an example
       of the CSV report imported to a Google Spreadsheet
       with pivot calculating avearage duration of 
       tests for every library.
     - spell check
 - simple UI (command line) with guiding messages
   for the user who runs the tests. Spend as little 
   efforts as possible on this task, to release quickly.
   4h
 - readme with explanation of the project goal and
   how to use it
   5h
 - change db format
   + test run as plist (:descr <descr> :run-results <run-results>)
     instead of just (<descr> <run-results>)
   + run-results as a list instead of plist; libname
     which was a plist key is now a property of the lib-result 
     object. It is more convenient for standard mapping functions, 
     instead of current do-lib-results.
 - add more libraries: total number of 20 libraries
   is enough for the beginning
 + when loading of a library or library test system
   fails, ensure we have the error description in the output
   0.5h
 + The "thank you" message: where exactly to submit test results?
   Specify an email or issue tracker of the cl-test-grid project.
 + how to store public (central) database and failed library 
   outputs (files).
   An appealing way is to store it in the same git repository 
   on github, but with the std-out files the repository will 
   quickly grow to an unconvenient size (for new people the
   checkout procedure will be too long to be considered
   convenient)
   5h
   Solution: files are stored in Google App Engine blob store.
 - run the tests on all the implementations available for us.
 - usocket test suite might need manual configuration,
   see their README. Distinguish the case 
   when the manual configuration hasn't been
   performed and return :no-resource status.
 - For all the libraries which need manual configuration
   (cffi, usocket) provide guiding message to the
   user how to configure them.
 - finalize the decision what command user runs
   to performs the tests. Describe this main command
   in the README (in the first paragraph).

==================================================
==========    Milestone: release 0    ============
==================================================
 - finalize the terminology we use in the code
   to refer our main data: 
   - test status for a particular library
   - library test result object (includes the status 
     as well as log length, the key of the log
     in the online blob store, probably the
     library test duration)
   - list of library test results in a particular test 
     run
   - test run description, consists of lisp name,
     libraries set (think quicklisp distro),
     the user contacts, total test run duration,
     etc.
 - watchdog for hanging tests
 + more abstract accessor to parts of DB info instead of
   getf by properties: run-descr, run-results.
   1h
 + safe-read database
 + create a project with asdf system
   0.5h
 + DB file path based on the asdf system location
   0.5h
 + accumulate failed library output
   1h
 - DB file formatting should be equal in all lisps,
   so that diff shows only new records.
   (use pprint ?)
   4h
 - a way to specify lib-wold as a quicklisp version with some 
   library versions overriden (checkout this particular 
   libraries from the scm), so that library author can quickly 
   get test result for his changes (fixes)  in scm. 
   An implementation idea to consider: almost every scm allows 
   to download asnapshot via http, so the quicklisp http machinery may
   be reused here, whithout running a shell command for 
   checkout.
   24h
 - should we save library log to a file only if the tests failed, 
   or always? (now we save log in any case)
 - During run-libtests, probably we should redirect the library
   output to file directly, without caching it in memory
   - it is more convenient when you are watching the testing
   process, you can observe the file being populated with 
   logs (because some libraries, like flexi-streams, take 
   time about minute to finish, and if during this minute
   nithing happens it is not user-friendly)
|#

(defgeneric libtest (library-name)
  (:documentation "Define a method for this function
with LIBRARY-NAME eql-specialized for for every library added 
to the test grid.

The method should run test suite and return the resulting
status. Status is one of three values: 
:OK - all tests passed,
:FAIL - some test failed,
:NO-RESOURCE - test suite can not be run because some required 
resource is absent in the environment. For example, CFFI library
test suite needs a small C library compiled to DLL. User must
do it manually. In case the DLL is absent, the LIBTEST method
for CFFI returns :NO-RESOURCE.

For convenience, T may be returned instead of :OK and NIL instead of :FAIL."))

(defun normalize-status (status)
  "Normilzies test resul status - converts T to :OK and NIL to :FAIL."
  (case status
    ((t :ok) :ok)
    ((nil :fail) :fail)
    (otherwise status)))

(defparameter *all-libs* '(:alexandria :babel :trivial-features :cffi 
                           :cl-ppcre :usocket :flexi-streams :bordeaux-threads
                           :cl-base64 :trivial-backtrace :puri :anaphora
                           :parenscript :trivial-garbage :iterate :metabang-bind
                           :cl-json)
  "All the libraries currently supported by the test-grid.")

(defun clean-rt ()
  "Helper function to assist running test suites created using the RT 
test framework. The problem is that RT uses global storage for all
the tests; in result if we previously loaded any test system,
after loading another test system the global test RT test suite
contains the tests of _both_ libraries."
  (let ((rem-all-tests (and (find-package '#:rt)
                            (find-symbol (symbol-name '#:rem-all-tests) '#:rt))))
    (when rem-all-tests (funcall rem-all-tests))))
  
(defmethod libtest ((library-name (eql :alexandria)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :alexandria-tests)

  (quicklisp:quickload :alexandria-tests)

  (flet (
         ;; the run-tests local function is copy/pasted 
         ;; from alexandria-tests.asd         
         (run-tests (&rest args)
           (apply (intern (string '#:run-tests) '#:alexandria-tests) args)))

    (let ((a (run-tests :compiled nil))
          (b (run-tests :compiled t)))
      (and a b))))

(defmethod libtest ((library-name (eql :babel)))
  
  ;; The test framework used: stefil.
  
  (quicklisp:quickload :babel-tests)
  
  (let ((result (funcall (intern (string '#:run) '#:babel-tests))))
    (zerop 
     (length (funcall (intern (string '#:failure-descriptions-of) '#:hu.dwim.stefil)  
                      result)))))

(defmethod libtest ((library-name (eql :trivial-features)))
  
  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :trivial-features-tests)

  (quicklisp:quickload :trivial-features-tests)
  
  ;; copy/past from trivial-features-tests.asd
  (let ((*package* (find-package 'trivial-features-tests)))
    (funcall (find-symbol (symbol-name '#:do-tests)))))

(defmethod libtest ((library-name (eql :cffi)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :cffi-tests)

  (handler-case (quicklisp:quickload :cffi-tests)
    ;; CFFI tests work with a small test C 
    ;; library. The user is expected to compile
    ;; the library. If the library is not available,
    ;; CFFI tests signal cffi:load-foreign-library-error.
    (t (e)
      (when (eq (type-of e) 
                (find-symbol (symbol-name '#:load-foreign-library-error) '#:cffi))
        (return-from libtest :no-resource))))

  (flet (
         ;; copy/paste from cffi-tests.asd
         (run-tests (&rest args)
           (apply (intern (string '#:run-cffi-tests) '#:cffi-tests) args)))

    (let ((a (run-tests :compiled nil))
          (b (run-tests :compiled t)))
      (and a b))))

(defmethod libtest ((library-name (eql :cl-ppcre)))

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
  
  ;; copy/paste from usocket-test.asd 
  (funcall (intern "DO-TESTS" "USOCKET-TEST")))
  

(defmethod libtest ((library-name (eql :flexi-streams)))

  ;; The test framework used: custom.

  (quicklisp:quickload :flexi-streams-test)

  ;; copy/paste from flexi-streams.asd 
  (funcall (intern (symbol-name :run-all-tests)
                   (find-package :flexi-streams-test))))

(defun run-fiveam-suite (fiveam-test-spec)
  "Runs the specified test suite created with the FiveAM 
test framework. Returns T if all the tests succeeded and
NIL otherwise. The FIVEAM-TEST-SPEC specifies the tests
suite according to the FiveAM convention."
  (let ((run (intern (string '#:run) :fiveam))
        (explain (intern (string '#:explain) :fiveam))
        (detailed-text-explainer (intern (string '#:detailed-text-explainer) :fiveam))
        (test-failure-type (intern (string '#:test-failure) :fiveam)))
    
    (let ((results (funcall run fiveam-test-spec)))
      (funcall explain (make-instance detailed-text-explainer) results *standard-output*)
      (zerop (count-if (lambda (res)
                         (typep res test-failure-type))
                       results)))))

(defmethod libtest ((library-name (eql :bordeaux-threads)))

  ;; The test framework used: fiveam.
  
  (quicklisp:quickload :bordeaux-threads-test)

  (run-fiveam-suite :bordeaux-threads))

(defmethod libtest ((library-name (eql :cl-base64)))

  ;; The test framework used: ptester.
  
  (quicklisp:quickload :cl-base64-tests)
  
  (funcall (intern (symbol-name '#:do-tests)
                   (find-package '#:cl-base64-tests))))

(defun lift-tests-ok-p (lift-tests-result)
  "Helper function to work with Lift test framework.
Examines the tests result object and retuns T is all
the tests are successull and NIL otherwise."
  (let ((errors (intern (symbol-name '#:errors) :lift))
        (expected-errors (intern (symbol-name '#:expected-errors) :lift))
        (failures (intern (symbol-name '#:failures) :lift))
        (expected-failures (intern (symbol-name '#:expected-failures) :lift)))
    (zerop
     (+ (length (set-difference (funcall errors lift-tests-result)
                                (funcall expected-errors lift-tests-result)))
        (length (set-difference (funcall failures lift-tests-result)
                                (funcall expected-failures lift-tests-result)))))))

(defun run-lift-tests (suite-name)
  "Helper function to work with the Lift test framework.
Runs the specified Lift test suite and returns T
if all the tests succeeded and NIL othersize."
  (let ((result (funcall (intern (symbol-name '#:run-tests) :lift)
                         :suite suite-name)))
    (describe result *standard-output*)
    (lift-tests-ok-p result)))
  
(defmethod libtest ((library-name (eql :trivial-backtrace)))
  
  ;; The test framework used: lift.
  
  (quicklisp:quickload :trivial-backtrace-test)

  (run-lift-tests :trivial-backtrace-test))

(defmethod libtest ((library-name (eql :puri)))

  ;; The test framework used: ptester.

  (quicklisp:quickload :puri-tests)
  
  ;; copy/paste from puri.asd
  (funcall (intern (symbol-name '#:do-tests)
                   (find-package :puri-tests))))

(defmethod libtest ((library-name (eql :anaphora)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :anaphora-test)

  (quicklisp:quickload :anaphora-test)

  ;; copy/paste from anaphora.asd
  (funcall (intern "DO-TESTS" :rt)))

(defmethod libtest ((library-name (eql :parenscript)))
  ;; The test framework used: eos (similar to FiveAM).

  ;; asdf:test-op is not provided for parenscript,
  ;; only a separate package ps-test with public
  ;; function run-tests.

  ;; The test suites to run determined by looking
  ;; into the function run-tests in the file test.lisp.
  (let* ((run (intern (string '#:run) :eos))
         (test-failure-type (intern (string '#:test-failure) :eos))
         (results (append (funcall run (intern (string '#:output-tests) :ps-test))
                          (funcall run (intern (string '#:package-system-tests) :ps-test)))))  
    (zerop (count-if (lambda (res)
                       (typep res test-failure-type))
                     results))))

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
  
  (funcall (find-symbol (string '#:do-tests) '#:rtest)))

(defmethod libtest ((library-name (eql :iterate)))

  ;; The test framework used: rt.
  (clean-rt)
  (asdf:clear-system :iterate-tests)
  (asdf:clear-system :iterate)

  (quicklisp:quickload :iterate-tests)
  
  (funcall (intern "DO-TESTS" (find-package #+sbcl "SB-RT"
                                            #-sbcl "REGRESSION-TEST"))))

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

  (run-lift-tests :metabang-bind-test))

(defmethod libtest ((library-name (eql :cl-json)))
  ;; The test framework used: fiveam.
  (quicklisp:quickload :cl-json.test)
  (run-fiveam-suite (intern (symbol-name '#:json) :json-test)))


(defun run-libtest (lib)
  (let* ((orig-std-out *standard-output*)
         (buf (make-string-output-stream))
         (*standard-output* buf)
         (*error-output* buf)
         (start-time (get-internal-real-time)))

    (format orig-std-out 
            "Running tests for ~A. *STANDARD-OUTPUT* and *ERROR-OUTPUT* are redirected.~%"
            lib)
    (finish-output orig-std-out)
    
    (let ((status (handler-case (normalize-status (libtest lib)) 
                    (t () :fail))))      
      (when (eq :fail status)
        (format t "~A tests failed." lib))
      (let ((output (get-output-stream-string buf)))
        (list :libname lib
              :status status :output output
              :log-char-length (length output) 
              :test-duration (/ (- (get-internal-real-time) start-time) 
                                internal-time-units-per-second))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; copy/paste from 
;; http://cl-user.net/asp/-1MB/sdataQ0mpnsnLt7msDQ3YNypX8yBX8yBXnMq=/sdataQu3F$sSHnB==
(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-settings-file()
 (merge-pathnames (user-homedir-pathname) "cl-test-grid-settings.lisp"))

(defun prompt-for-email ()
             (format *query-io* "~a: " "Please enter your email for questions about this, test, your environment, adds")
             (force-output *query-io*)
             (string-trim " " (read-line *query-io*)))

(defun get-user-email ()
  (LET ((USER-EMAIL nil))
    (handler-case
        (if (STRING= "" (setf user-email(GETF (SAFE-READ-FILE (GET-SETTINGS-FILE)
                                                              ) :USER-EMAIL)))
          (FORMAT t "Warning! Empty email is specified in the settings file ~a~%" (get-settings-file)))
        (t ()
           (PROGN
             (write-to-file (list :user-email (setf user-email (prompt-for-email)))
                                  (get-settings-file)))))
        user-email))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun save-lib-log (lib-name log test-run-directory)
  (let ((lib-log-file (lib-log-file test-run-directory lib-name)))
    (with-open-file (out lib-log-file
                         :direction :output
                         :if-exists :overwrite
                         :if-does-not-exist :create)
      (write-sequence log out))))

(defun write-to-file (obj file)
  "Write to file the lisp object OBJ in a format acceptable to READ."
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (pprint obj out))
  obj)

(defun run-info-file (test-run-directory)
  (merge-pathnames "test-run-info.lisp"
                   test-run-directory))

(defun save-run-info (test-run directory)
  (let ((run-file (run-info-file directory)))
    (write-to-file test-run run-file)))    

(defun gae-blobstore-dir ()
  (merge-pathnames "gae-blobstore/lisp-client/" test-grid-config:*src-base-dir*))

(defparameter *gae-blobstore-base-url* "http://cl-test-grid.appspot.com")

(defun get-blobstore ()
  (pushnew (truename (gae-blobstore-dir)) asdf:*central-registry* :test #'equal)
  (ql:quickload '#:test-grid-gae-blobstore)
  (funcall (intern (string '#:make-blob-store) '#:test-grid-gae-blobstore) 
           :base-url *gae-blobstore-base-url*))

(defun submit-logs (test-run-dir)
  (let* ((blobstore (get-blobstore))
         (run-info (safe-read-file (run-info-file test-run-dir)))
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
      (save-run-info run-info test-run-dir))))
  
(defun run-libtests (&optional (libs *all-libs*))
  (let* ((run-descr (make-run-descr))
         (run-dir (run-directory run-descr))
         (lib-results))
    (ensure-directories-exist run-dir)
    (dolist (lib libs)
      (let ((lib-result (run-libtest lib)))
        (save-lib-log lib (getf lib-result :output) run-dir)
        (remf lib-result :output)
        (push lib-result lib-results)))
    (setf (getf run-descr :run-duration) 
          (- (get-universal-time)
             (getf run-descr :time)))
    (let ((run (make-run run-descr lib-results)))
      (save-run-info run run-dir)
      (format t "The test results were saved to this directory: ~%~A.~%" 
              (truename run-dir))
      (format t "~%Submitting libraries test logs to the online blobstore...~%")
      (handler-case 
          (progn
            (setf run (submit-logs run-dir))
            (format t "The log files are successfully uploaded to the online blobstore.
 
Please submit the test run results file 
   ~A 
to the cl-test-grid issue tracker: 
   https://github.com/cl-test-grid/cl-test-grid/issues
 
 (we are working on automating the test results upload).~%"
                    (truename (run-info-file run-dir))))
        (t (e) (format t "Error occured while uploading the libraries test logs to the online store: ~A: ~A.
Please submit manually the full content of the results directory 
   ~A
to the cl-test-grid issue tracker: 
   https://github.com/cl-test-grid/cl-test-grid/issues~%"
                       (type-of e)
                       e
                       (truename run-dir))))
      (format t "~%Thank you for the participation!~%")
      run)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *db* '(:version 0 :runs ()))

(defvar *standard-db-file* 
  (merge-pathnames "db.lisp" 
                   test-grid-config:*src-base-dir*))

(defun add-run (run-info &optional (db *db*))
  (push run-info (getf db :runs)))

(defun save-db (&optional (db *db*) (stream-or-path *standard-db-file*))
  (with-open-file (out stream-or-path 
                       :direction :output 
                       :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (write db :stream out)))

(defun read-db (&optional (stream-or-path *standard-db-file*))
  (with-open-file (in stream-or-path 
                      :direction :input 
                      :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                      )
    (safe-read in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-fake-run-results ()
  "Generate fake test run result enought to test our reports."
  (flet ((random-status ()
           (let ((r (random 1.0)))
             (cond ((< r 0.43)
                    :ok)
                   ((< r 0.86)
                    :fail)
                   (t :no-resource)))))
    (let ((runs '()))
      (dolist (lisp '("sbcl-fake-1" "sbcl-fake-2" "clisp-fake-1" "ccl-fake-1" "abcl-fake-2"))
        (dolist (lib-world '("quicklisp-fake-2011-00-01" "quicklisp-fake-2011-00-02" "quicklisp-fake-2011-00-03"))
          (let ((run-descr (list :lisp lisp
                                 :lib-world lib-world
                                 :time (get-universal-time)
                                 :run-duration (+ 100 (random 90))
                                 :contact (list :email
                                                (nth (random 3) '("avodonosov@yandex.ru"
                                                                  "other-user@gmail.com"
                                                                  "foo@gmail.com")))))
                (lib-results '()))
            (dolist (lib *all-libs*)
              (push (list :libname lib :status (random-status) :log-char-length 50)
                    lib-results))
            (push (make-run run-descr lib-results) runs))))
      runs)))

(defvar *test-runs-report-template* 
  (merge-pathnames "test-runs-report-template.html"
                   test-grid-config:*src-base-dir*))

(defun vertical-html (libname) 
  (let ((maybeBr "")
        (libname (string libname)))
    (with-output-to-string (out)
      (loop for char across libname
         do (princ maybeBr out)
           (princ (if (char= char #\-) #\| char) out)
           (setf maybeBr "<br/>")))))
           
;; example:
#|
 (string= (vertical-html "cl-abc")
          "c<br/>l<br/>|<br/>a<br/>b<br/>c")
|#

;; todo: this should be a blobstore method, but
;; until we move the reporting to a separate
;; asdf system, we don't want the dependency 
;; on blobstore here.
(defun blob-uri (blob-key)
  (format nil "~A/blob?key=~A" 
          *gae-blobstore-base-url* blob-key))

(defun lib-log-local-uri (test-run lib-result)
  (format nil "file://~A~A"
          (run-directory (run-descr test-run))
          (string-downcase (getf lib-result :libname))))

(defun lib-log-uri (test-run lib-result)
  (declare (ignore test-run))
  (let ((blob-key (getf lib-result :log-blob-key)))
    (if blob-key
        (blob-uri blob-key)
        "javascript:alert('The blobstore key is not specified, seems like the library log was not submitted to the online storage')")))

(defun single-letter-status (normalized-status) 
  (case normalized-status
    (:ok "O")
    (:fail "F")
    (:no-resource "R")
    (otherwise normalized-status)))

(defun status-css-class (normalized-status) 
  (case normalized-status
    (:ok "ok-status")
    (:fail "fail-status")
    (:no-resource "no-resource-status")
    (otherwise "")))
           
(defun render-single-letter-status (test-run lib-test-result)
  (if (null lib-test-result)
      "&nbsp;"
      (let ((status (normalize-status (getf lib-test-result :status))))
        (format nil "<a class=\"test-status ~A\" href=\"~A\">~A</a>" 
                (status-css-class status)
                (lib-log-uri test-run lib-test-result)
                (single-letter-status status)))))

(defun test-runs-table-html (&optional 
                             (db *db*)
                             (status-renderer 'render-single-letter-status))
  (with-output-to-string (out)
    (write-line "<table cellspacing=\"1\" class=\"tablesorter\">" out)
    
    (princ "<thead><tr style=\"vertical-align: bottom;\"><th>Start Time</th><th>Lib World</th><th>Lisp</th><th>Runner</th>" out)
    (dolist (lib *all-libs*)
      (format out "<th>~A</th>" (vertical-html lib)))
    (write-line "</tr></thead>" out)
    
    (write-line "<tbody>" out)
    (dolist (run (getf db :runs))
      (let ((run-descr (run-descr run))
            (lib-statuses (run-results run)))
        (format out "<tr><td>~A</td><td>~A</td><td>~A</td><td>~A</td>" 
                (pretty-fmt-time (getf run-descr :time)) 
                (getf run-descr :lib-world) 
                (getf run-descr :lisp)
                (getf (getf run-descr :contact) :email))
        (dolist (lib *all-libs*)
          (format out "<td>~A</td>" 
                  (funcall status-renderer run (find lib lib-statuses 
                                                     :key (getter :libname)))))
        (write-line "</tr>" out)))
    (write-line "</tbody>" out)
    (write-line "</table>" out)))

(defun fmt-test-runs-report (html-table)
  (let* ((template (file-string *test-runs-report-template*))
         (placeholder "{THE-TABLE}")
         (pos (or (search placeholder template)
                  (error "Can't find the placeholder ~A in the report template file ~A" placeholder *test-runs-report-template*))))
    (concatenate 'string 
                 (subseq template 0 pos)
                 html-table
                 (subseq template (+ pos (length placeholder))))))

(defun test-runs-report (&optional (db *db*))
  (fmt-test-runs-report (test-runs-table-html db)))

(defun export-to-csv (out &optional (db *db*))
  (format out "Lib World,Lisp,Runner,LibName,Status,TestDuration~%")
  (dolist (run (getf db :runs))
    (let ((run-descr (run-descr run)))
      (dolist (lib-result (run-results run))
        (format out "~a,~a,~a,~a,~a,~a~%"
                (getf run-descr :lib-world)
                (getf run-descr :lisp)
                (getf (getf run-descr :contact) :email)
                (string-downcase (getf lib-result :libname))
                (getf lib-result :status)
                (getf lib-result :test-duration))))))

;; ========= Pivot Reports ==================

(defun build-joined-index (db)
  (let ((all-results (make-hash-table :test 'equal)))
    (dolist (run (getf db :runs))
      (let* ((run-descr (run-descr run))
             (lisp (getf run-descr :lisp))
             (lib-world (getf run-descr :lib-world)))
        (dolist (lib-result (run-results run))
          (let ((libname (getf lib-result :libname)))
            (push lib-result
                  (gethash (list lisp lib-world libname) all-results))))))
    all-results))


#|    
HTML table properties:
 - rows and cols are not equal: html tables are row-first - TR includes TDs

Only the deepest level row and col fields will have corresponding
TR and TD cells in the table data (we do not consider the table 
column headers now).

And the TD cells are included in a TR.

Algorithm sketch:
  Iterate over all row properties and their values in rows.
  For every deepest level combination create a TR (if 
  any values exist for this combination).

    Iterate over all the col properties and their values.
    For every deepest level combination create a TD.

partitioning structure
-----------------------------
    field1 | field2
-----------------------------
    field1 val1 (count)
             field2 val1 (count)
             field2 val2 (count)
    field1 val2 (count)
             field2 val1 (count)
             field2 val3 (count)
    field1 val3 (count)
             field2 val2 (count)


<tr> <td> <td> <td>
<tr>      <td> <td>
<tr>           <td>
<tr>      <td> <td>
<tr>      <td> <td>
<tr>           <td>
<tr> <td> <td> <td>
<tr>      <td> <td>
<tr> <td> <td> <td>
<tr>      <td> <td>
<tr>           <td>

|#

(defun make-fields-values-setter (fields)
  "Creates a function which destructively modifies
the specified fields in the index key passed to it 
as a parameter"
  (let ((index-key-setters (list :lisp #'(lambda (index-key lisp)
                                           (setf (first index-key) lisp))
                                 :lib-world #'(lambda (index-key lib-world)
                                                (setf (second index-key) lib-world))
                                 :libname #'(lambda (index-key libname)
                                              (setf (third index-key) libname)))))
    (flet ((field-setter (field)
             (or (getf index-key-setters field)
                 (error "field ~A is unknown" field))))
      (let ((setters (mapcar #'field-setter fields)))
        #'(lambda (index-key field-vals)
            (mapc #'(lambda (setter field-val)
                      (funcall setter index-key field-val))
                  setters
                  field-vals)
            index-key)))))

(defun make-fields-values-getter (fields)
  (let ((index-key-getters (list :lisp #'first
                                 :lib-world #'second
                                 :libname #'third)))
    (flet ((field-getter (field)
             (or (getf index-key-getters field)
                 (error "field ~A is unknown" field))))
      (let ((getters (mapcar #'field-getter fields)))
        #'(lambda (index-key)
            (mapcar #'(lambda (getter)
                        (funcall getter index-key))
                    getters))))))

(defun calc-rows-and-cols (joined-index rows-fields cols-fields)
  (let ((rows-fields-getter (make-fields-values-getter rows-fields))
        (rows-fields-setter (make-fields-values-setter rows-fields))
        (rows (make-hash-table :test #'equal))
        (cols-fields-getter (make-fields-values-getter cols-fields))
        (cols-fields-setter (make-fields-values-setter cols-fields))
        (cols (make-hash-table :test #'equal)))
    (maphash #'(lambda (index-key index-value)
                 (declare (ignore index-value))
                 (setf (gethash (funcall rows-fields-getter index-key)
                                rows)
                       t)
                 (setf (gethash (funcall cols-fields-getter index-key)
                                cols)
                       t))
             joined-index)
    (values (hash-table-keys rows)
            (hash-table-keys cols)
            #'(lambda (index-key row-addr col-addr)
                (funcall rows-fields-setter index-key row-addr)
                (funcall cols-fields-setter index-key col-addr)))))

(defstruct (header-print-helper :conc-name)
  (span 0 :type fixnum)
  (printed nil))

(defun subaddrs (row-address)
  (nreverse (maplist #'reverse (reverse row-address))))

(defun calc-spans (row-or-col-addrs)
  (let ((helpers (make-hash-table :test #'equal)))
    (dolist (row-or-col-addr row-or-col-addrs)
      (dolist (subaddr (subaddrs row-or-col-addr))
        (let ((helper (or (gethash subaddr helpers)
                          (setf (gethash subaddr helpers)
                                (make-header-print-helper)))))
          (incf (span helper)))))
    helpers))
         
(defun print-row-header (row-addr row-spans out)
  (dolist (subaddr (subaddrs row-addr))
    (let ((helper (gethash subaddr row-spans)))
      (when (not (printed helper))
        (format out "<td rowspan=\"~A\">~A</td>" (span helper) (car (last subaddr)))
        (setf (printed helper) t)))))

(defun print-table-headers (row-field-count col-field-count cols out)
  (let ((col-spans (calc-spans cols)))
    (dotimes (header-row-num col-field-count)
      (princ "<tr>" out)
      (dotimes (row-header row-field-count)
        (princ "<td>&nbsp;</td>" out))
      (dolist (col-addr cols)
        (let* ((cell-addr (subseq col-addr 0 (1+ header-row-num)))
               (helper (gethash cell-addr col-spans)))
          (when (not (printed helper))
            (format out "<td colspan=\"~A\">~A</td>" (span helper) (car (last cell-addr)))
            (setf (printed helper) t))))
      (format out "</tr>~%"))))

(defun pivot-table-html (out 
                         joined-index
                         row-fields row-fields-sort-predicates
                         col-fields col-fields-sort-predicates)
  (princ "<table border=\"1\">" out)
  (let (rows
        cols
        index-key-setter
        (row-comparator #'(lambda (rowa rowb)
                            (list< row-fields-sort-predicates
                                   rowa rowb)))
        (col-comparator #'(lambda (cola colb)
                            (list< col-fields-sort-predicates
                                   cola colb))))

    (setf (values rows cols index-key-setter) 
          (calc-rows-and-cols joined-index row-fields col-fields))
    
    (setf rows (sort rows row-comparator)
          cols (sort cols col-comparator))
    
    (print-table-headers (length row-fields) (length col-fields) cols out)
    (let ((row-spans (calc-spans rows))
          (index-key (make-sequence 'list (+ (length row-fields)
                                             (length col-fields)))))
      (dolist (row rows)
        (princ "<tr>" out)
        (print-row-header row row-spans out)
        (dolist (col cols)
          (funcall index-key-setter index-key row col)
          (let ((data (gethash index-key joined-index)))
            (format out "<td>~A</td>" data)))      
        (format out "</tr>~%"))))
    (princ "</table>" out))
  
(defun print-pivot-reports (db)
  (let ((joined-index (build-joined-index db))
        (reports-dir (reports-dir)))
    (flet ((print-report (filename 
                          row-fields row-fields-sort-predicates
                          col-fields col-fields-sort-predicates)
             (with-open-file (out (merge-pathnames filename reports-dir)
                                  :direction :output 
                                  :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
               (pivot-table-html out 
                                 joined-index
                                 row-fields row-fields-sort-predicates
                                 col-fields col-fields-sort-predicates))))
      
      (print-report "pivot_ql_lisp-lib.html"
                    '(:lib-world) (list #'string<)
                    '(:lisp :libname) (list #'string< #'string<))
      (print-report "pivot_ql_lib-lisp.html"
                    '(:lib-world) (list #'string<)
                    '(:libname :lisp) (list #'string< #'string<))
      
      (print-report "pivot_lisp_lib-ql.html"
                    '(:lisp) (list #'string<)
                    '(:libname :lib-world) (list #'string< #'string<))
      (print-report "pivot_lisp_ql-lib.html"
                    '(:lisp) (list #'string<)
                    '(:lib-world :libname) (list #'string< #'string<))
      
      (print-report "pivot_lib_lisp-ql.html"
                    '(:libname) (list #'string<)
                    '(:lisp :lib-world) (list #'string< #'string<))
      (print-report "pivot_lib_ql-lisp.html"
                    '(:libname) (list #'string<)
                    '(:lib-world :lisp) (list #'string< #'string<))
      
      (print-report "pivot_ql-lisp_lib.html"
                    '(:lib-world :lisp) (list #'string<)
                    '(:libname) (list #'string< #'string<))
      (print-report "pivot_ql-lib_lisp.html"
                    '(:lib-world :libname) (list #'string<)
                    '(:lisp) (list #'string< #'string<))
      
      (print-report "pivot_lisp-lib_ql.html"
                    '(:lisp :libname) (list #'string<)
                    '(:lib-world) (list #'string< #'string<))
      (print-report "pivot_lisp-ql_lib.html"
                    '(:lisp :lib-world) (list #'string<)
                    '(:libname) (list #'string< #'string<))
      
      (print-report "pivot_lib-lisp_ql.html"
                    '(:libname :lisp) (list #'string<)
                    '(:lib-world) (list #'string< #'string<))
      (print-report "pivot_lib-ql_lisp.html"
                    '(:libname :lib-world) (list #'string<)
                    '(:lisp) (list #'string< #'string<)))))

(defun generate-reports (&optional (db *db*))
                          
  (with-open-file (out (merge-pathnames "test-runs-report.html"
                                        (reports-dir))
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-sequence (test-runs-report db) out))
  
  (with-open-file (out (merge-pathnames "export.csv"
                                        (reports-dir))
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (export-to-csv out))
  
  (print-pivot-reports db))

(defun reports-dir () 
  (merge-pathnames "reports-generated/"
                   test-grid-config:*src-base-dir*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Quicklisp download statistics:

http://blog.quicklisp.org/2010/11/project-download-statistics.html

colunmns: download count, has common-lisp test suite (as of quicklisp 2011-07-30).

    714 + alexandria
    596 + babel
    520 + trivial-features
    503 + cffi
    450 + cl-ppcre
    423 - trivial-gray-streams
    404 + usocket
    403 + flexi-streams
    398 + bordeaux-threads
    393 - slime
    386 - cl+ssl (thre is a test.lisp, but it's not automated, and no (asdf:operate (op asdf:test-op) ...)
    371 - chunga
    370 + cl-base64
    361 - cl-fad
    339 - md5
    327 - quicklisp-slime-helper
    323 + trivial-backtrace
    321 - rfc2388 (there is a test.lisp, but there is no asdf:test-op, and the code in test.lisp 
                   doesn't return fail/ok status, it jsut prints something to the console)
    317 - hunchentoot (there are tests and asdf:test-op, but I am affrait it might take
                       lot of work to automate it: test-op starts server and doesn't
                       stop; I am also afraid it might hang sometimes; implementation
                       would also require checking for single-threaded lisps
                       (by hunchentoot::*supports-threadss-p* ?)
                       and returning :no-resource. Leave hunchentoot for a later
                       stage)
    293 - salza2
    289 + puri
    285 - closer-mop (no asdf:test-op. there is a folder "test" with some file jeffs-code.lisp,
                      but it's a code to reproduce some particular issue. It does not seem
                      to be intended for automated regression testing of closer-mop)
    225 + anaphora
    224 + parenscript
    221 - cl-who
    207 + trivial-garbage
    201 + iterate
    193 - cl-vectors
    190 - zpng
    177 - asdf-system-connections
    174 - zpb-ttf
    173 + uffi But the test suite is non trivial (for example, it defines asdf:compile-op 
               for C files using make). Probably that's why quickisp does not
               make the uffi-tests.asd availabel for ql:quickload. A study is needed about 
               how to include this system, therefore I avoid it for now.
    173 + metabang-bind
    170 - split-sequence
    164 - vecto (there is a test.lisp, but it's not automated, intended for manual run and eye-testing of the resulting images)
    163 + cl-json
  ------------------------ << I am here
    162 + cl-containers
    161 + metatilities-base
    159 - fare-utils
    156 + weblocks (do these tests start hunchentoot? seems no, it creates mock objects for request, response, etc.)
    156 - fare-matcher (no test-op, but there is fare-matcher-test.asd with one test defined using stefil)
    148 - drakma
    144 + cl-cont
    143 - closure-common
    140 + moptilities
    138 - f-underscore
    137 + trivial-timeout
    136 + metatilities
    135 + clsql (big test suite, requires database server(s). Runs test on the 
                 DB servers you specified in the configiration. Therefore
                 we need to think how to represent results - we can't
                 just collect results under the same name "clsql", because 
                 different agents might have tested different servers.
                 Conclusion: very useful test suite to include into 
                 our test set, but we will do it later).
    133 + cxml (but how to run it? seems like it requires some .xml 
                files which are not in the repository, so it probably
                is not fully automated).
|#
