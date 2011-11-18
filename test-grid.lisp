(defpackage #:test-grid (:use :cl))

(in-package #:test-grid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

TODO: 
  - information about test run: 
   + lisp-version-string, 
   + lib-world, 
   - author contact (get it from some settings file), 
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
     - use the online blob URL in the report
   - CSV export of the database to use it then with spreadsheets,
     google fusion tables, etc. and format as a pivot for 
     various projections (by quicklisp releases, by
     platform, etc)
     5h
   - an informer which may be embedded into a library
     project page, with reports about the test statuses 
     for this single library on various platforms with
     various quicklisp versions
 - simple UI (command line) with guiding messages
   for the user who runs the tests. Spend as little 
   efforts as possible on this task, to release quickly.
   4h
 - readme with explanation of the project goal and
   how to use it
   5h
 - change db format
   - test run as plist (:descr <descr> :run-results <run-results>)
     instead of just (<descr> <run-results>)
   - run-results as alist instead of plist (more convenient
     for standard mapping functions, instead of current do-lib-results)
 - add more libraries: total number of 20 libraries
   is enough for the beginning
 - when loading of a library or library test system
   fails, ensure we have the error description in the output
   0.5h
 - The "thank you" message: where exactly to submit test results?
   Specify an email or issue tracker of the cl-test-grid project.
 - how to store public (central) database and failed library 
   outputs (files).
   An appealing way is to store it in the same git repository 
   on github, but with the std-out files the repository will 
   quickly grow to an unconvenient size (for new people the
   checkout procedure will be too long to be considered
   convenient)
   5h
 - run the tests on all the implementations available for us.
==================================================
==========    Milestone: release 0    ============
==================================================
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
with LIBRARY-NAME eql-specialize for for every library added 
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

(defparameter *all-libs* '(:alexandria :babel :trivial-features :cffi :cl-ppcre :usocket :flexi-streams :bordeaux-threads)
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

(defmethod libtest ((library-name (eql :bordeaux-threads)))

  ;; The test framework used: fiveam.
  
  (quicklisp:quickload :bordeaux-threads-test)
  
  (let ((results (funcall (intern (string '#:run) :fiveam)
                          :bordeaux-threads))
        (test-failure-type (intern (string '#:test-failure) :fiveam)))
        
    (zerop (count-if (lambda (res)
                       (typep res test-failure-type))
                     results))))

(defun run-libtest (lib)
  (let* ((orig-std-out *standard-output*)
         (buf (make-string-output-stream))
         (*standard-output* buf)
         (*error-output* buf))

    (format orig-std-out "Running tests for library ~A. *STANDARD-OUTPUT* and *ERROR-OUTPUT* are redirected.~%"
            lib)
    (finish-output orig-std-out)
    
    (let ((status (handler-case (normalize-status (libtest lib)) 
                    (t () :fail))))      
      (when (eq :fail status)
        (format t "~A tests failed." lib))
      (let ((output (get-output-stream-string buf)))
        (list :status status :output output
              :log-char-length (length output))))))

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
;; Test Runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-descr (run)
  "The description part of the test run."
  (first run))

(defun run-results (run)
  "The list of test suite statuses for every library in the specified test run."
  (second run))

(defun (setf run-results) (new-run-results test-run)
  (setf (second test-run) new-run-results))

(defmacro do-lib-results ((lib lib-result run-results) &body body)
  `(do-plist (,lib ,lib-result ,run-results) ,@body))

;; these two functions are not used now, maybe delete them

(defun save-run-logs (run directory)
  (ensure-directories-exist directory)
  (let ((lib-results (second run)))
    (do-lib-results (lib lib-result lib-results)
      (let ((lib-output (getf lib-result :output))
            (lib-log-file (merge-pathnames (string-downcase lib)
                                            directory)))
        (with-open-file (out lib-log-file
                             :direction :output
                             :if-exists :overwrite
                             :if-does-not-exist :create)
          (write-sequence lib-output out))))))

(defun delete-output (run)
  (let ((lib-results (second run)))
    (do ((cur lib-results (cddr cur)))
        ((null cur))
      (remf (second cur) :output)))
  run)

;; end of the two functions to be deleted

(defun fmt-time (universal-time &optional destination)
  "The preferred time format used in the cl-test-grid project."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time 0)
    (funcall #'format 
             destination 
             "~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D" 
             year month date hour min sec)))

(defun make-run-descr () 
  "Generate a description for a test run which might be 
performed in the current lisp system."
  (list :lisp (asdf::implementation-identifier)
        :lib-world (format nil "quicklisp ~A" 
                           (ql-dist:version (ql-dist:dist "quicklisp")))
        :time (get-universal-time)
        :run-duration :unknown
        :contact (list :email "avodonosov@yandex.ru")))

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
         (submit-params '()))
    ;; prepare parameters for the SUBMIT-FILES blobstore function
    (do-lib-results (lib lib-result (run-results run-info))
      (declare (ignore lib-result))
      (push (cons lib 
                  (lib-log-file test-run-dir lib))
            submit-params))
    ;; submit files to the blobstore and receive their blobkeys
    ;; in response
    (let* ((libname-to-blobkey-alist (test-grid-blobstore:submit-files blobstore submit-params)))
      ;; Now store the blobkeys for every library in the run-info.
      ;; Note, we destructively modify parts of the previously
      ;; read run-info.
      (flet ((get-blob-key (lib)
               (or (cdr (assoc lib libname-to-blobkey-alist))
                   (error "blobstore didn't returned blob bey for the log of the ~A libary" lib))))
        (let ((new-run-results '()))
          (do-lib-results (lib lib-result (run-results run-info))
            (setf (getf lib-result :log-blob-key) (get-blob-key lib))
            (push lib-result new-run-results)
            (push lib new-run-results))
          (setf (run-results run-info) new-run-results)
          (save-run-info run-info test-run-dir)
          run-info)))))

(defun run-libtests (&optional (libs *all-libs*))
  (let* ((run-descr (make-run-descr))
         (run-dir (run-directory run-descr))
         (lib-results))
    (ensure-directories-exist run-dir)
    (dolist (lib libs)
      (let ((lib-result (run-libtest lib)))
        (save-lib-log lib (getf lib-result :output) run-dir)
        (remf lib-result :output)
        (setf (getf lib-results lib) lib-result)))
    (setf (getf run-descr :run-duration) 
          (- (get-universal-time)
             (getf run-descr :time)))
    (let ((run (list run-descr lib-results)))
      (save-run-info run run-dir)
      (format t "The test results were saved to this directory:
  ~A.~%" (truename run-dir)))
    (format t "~%Submitting libraries test logs to the online blobstore...~%")
    (handler-case 
        (progn 
          (submit-logs run-dir)
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
    (format t "~%Thank you for the participation!~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *db* '(:version 0 :runs ()))

(defvar *standard-db-file* 
  (merge-pathnames "db.lisp" 
                   test-grid-config:*src-base-dir*))

(defun add-run (run-info &optional (db *db*))
  (push run-info (getf db :runs)))

(defun save-db (&optional (stream-or-path *standard-db-file*) (db *db*))
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
              (setf (getf lib-results lib)
                    (list :status (random-status) :log-char-length 50)))
            (push (list run-descr lib-results) runs))))
      runs)))

(defvar *report-template* 
  (merge-pathnames "report-template.html"
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

(defun lib-log-uri (test-run lib-name)
  (format nil "file://~A~A" 
          (run-directory (run-descr test-run))
          (string-downcase lib-name)))

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
           
(defun render-single-letter-status (test-run lib-name lib-test-result)
  (let ((status (normalize-status (getf lib-test-result :status))))
    (format nil "<a class=\"test-status ~A\" href=\"~A\">~A</a>" 
            (status-css-class status)
            (lib-log-uri test-run lib-name)
            (single-letter-status status))))

(defun summary-table-html (&optional 
                           (status-renderer 'render-single-letter-status)
                           (db *db*))
  (with-output-to-string (out)
    (write-line "<table cellspacing=\"1\" class=\"tablesorter\">" out)
    
    (princ "<thead><tr style=\"vertical-align: bottom;\"><th>Lib World</th><th>Lisp</th><th>Runner</th>" out)
    (dolist (lib *all-libs*)
      (format out "<th>~A</th>" (vertical-html lib)))
    (write-line "</tr></thead>" out)
    
    (write-line "<tbody>" out)
    (dolist (run (getf db :runs))
      (let ((run-descr (run-descr run))
            (lib-statuses (run-results run)))
      (format out "<tr><td>~A</td><td>~A</td><td>~A</td>" 
              (getf run-descr :lib-world) 
              (getf run-descr :lisp)
              (getf (getf run-descr :contact) :email))
      (dolist (lib *all-libs*)
        (format out "<td>~A</td>" 
                (funcall status-renderer run lib (getf lib-statuses lib))))
      (write-line "</tr>" out)))
    (write-line "</tbody>" out)
    (write-line "</table>" out)))

(defun fmt-report (html-table)
  (let* ((template (file-string *report-template*))
         (placeholder "{THE-TABLE}")
         (pos (or (search placeholder template)
                  (error "Can't find the placeholder ~A in the report template file ~A" placeholder *report-template*))))
    (concatenate 'string 
                 (subseq template 0 pos)
                 html-table
                 (subseq template (+ pos (length placeholder))))))

(defun export-to-csv (out &optional
                      (db *db*))
 
 (format out "Lib World,Lisp,Runner,LibName,Status~%")
  (dolist (run (getf db :runs))
    (do-plist (key value (run-results run))
      (format out "~a,~a,~a,~a,~a~%"
                     (getf (run-descr run) :lib-world)
                     (getf (run-descr run) :lisp) 
                     (getf (getf (run-descr run) :contact) :email)
                     (string-downcase key) 
                     (getf value :status)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Quicklisp download statistics:

http://blog.quicklisp.org/2010/11/project-download-statistics.html

colunmns: download count, has common-lisp test suite

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
    386 cl+ssl
    371 chunga
    370 cl-base64
    361 cl-fad
    339 md5
    327 quicklisp-slime-helper
    323 trivial-backtrace
    321 rfc2388
    317 hunchentoot
    293 salza2
    289 puri
    285 closer-mop
    225 anaphora
    224 parenscript
    221 cl-who
    207 trivial-garbage
    201 iterate
    193 cl-vectors
    190 zpng
    177 asdf-system-connections
    174 zpb-ttf
    173 uffi
    173 metabang-bind
    170 split-sequence
    164 vecto
    163 cl-json
    162 cl-containers
    161 metatilities-base
    159 fare-utils
    156 weblocks
    156 fare-matcher
    148 drakma
    144 cl-cont
    143 closure-common
    140 moptilities
    138 f-underscore
    137 trivial-timeout
    136 metatilities
    135 clsql
    133 cxml
|#
