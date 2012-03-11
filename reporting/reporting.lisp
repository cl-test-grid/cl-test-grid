(defpackage #:test-grid-reporting
  (:use :cl))

(in-package #:test-grid-reporting)

;; -------------- the reporting source code directory -----------;; 

(defun src-dir ()
  (merge-pathnames "reporting/"
                   test-grid-config:*src-base-dir*))

;; ------ file system location for generated reports ------

(defun reports-dir ()
  (merge-pathnames "reports-generated/"
                   test-grid-config:*src-base-dir*))

(defun with-report-file-impl (filename handler-func)
  (let ((reports-dir (reports-dir)))
    (with-open-file (out (merge-pathnames filename reports-dir)
                         :direction :output
                         :element-type 'character ;'(unsigned-byte 8) + flexi-stream
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (funcall handler-func out))))

(defmacro with-report-file ((out-stream-var filename) &body body)
  `(with-report-file-impl ,filename #'(lambda (,out-stream-var) ,@body)))

;; -------------  Templating ---------------------;;
;; (will be replaced by cl-closure-templates or html-template
;; after the reports are moved to a separate ASDF system).

(defun replace-str (template placeholder value)
  "Differs from CL:REPLACE in that placeholder and value may be of different length."
  (let* ((pos (or (search placeholder template)
                  (error "Can't find the placeholder ~A in the template." placeholder))))
    (concatenate 'string
                 (subseq template 0 pos)
                 value
                 (subseq template (+ pos (length placeholder))))))

(defun fmt-template (file substitutions-alist)
  (let ((template (test-grid::file-string file)))
    (dolist (subst substitutions-alist)
      (setf template (replace-str template (car subst) (cdr subst))))
    template))

;; ------ fake test results to test templating ------
;; (outdated because rarely used - we now have enought real data in DB)

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
            (dolist (lib test-grid::*all-libs*)
              (push (list :libname lib :status (random-status) :log-char-length 50)
                    lib-results))
            (push (test-grid::make-run run-descr lib-results) runs))))
      runs)))

;; ===================== Test Runs Report ====================
(defvar *test-runs-report-template*
  (merge-pathnames "test-runs-report-template.html"
                   (src-dir)))

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
          test-grid::*gae-blobstore-base-url* blob-key))

(defun lib-log-local-uri (test-run lib-result)
  (format nil "file://~A~A"
          (test-grid::run-directory (test-grid::run-descr test-run))
          (string-downcase (getf lib-result :libname))))

(defun lib-log-uri (lib-result)
  (let ((blob-key (getf lib-result :log-blob-key)))
    (if blob-key
        (blob-uri blob-key)
        "javascript:alert('The blobstore key is not specified, seems like the library log was not submitted to the online storage')")))

(defun aggregated-status (normalized-status)
  "Returns the test resutl as one symbol, even
if it was an \"extended status\". Possible return
values: :OK, :UNEXPECTED-OK, :FAIL, :NO-RESOURSE, :KNOWN-FAIL."
  (etypecase normalized-status
    (symbol normalized-status)
    (list (destructuring-bind (&key failed-tests known-to-fail) normalized-status
            (cond ((null failed-tests)
                   (if (null known-to-fail)
                       :ok
                       :unexpected-ok))
                  ((test-grid::set= failed-tests known-to-fail :test #'string=)
                   :known-fail)
                  (t :fail))))))

(defun single-letter-status (aggregated-status)
  (case aggregated-status
    (:ok "O")
    (:unexpected-ok "U")
    (:fail "F")
    (:known-fail "K")
    (:no-resource "R")
    (otherwise aggregated-status)))

(defun status-css-class (aggregated-status)
  (case aggregated-status
    (:ok "ok-status")
    ((:known-fail :fail :unexpected-ok) "fail-status")
    (:no-resource "no-resource-status")
    (otherwise "")))

(defun render-single-letter-status (test-run lib-test-result)
  (declare (ignore test-run))
  (if (null lib-test-result)
      "&nbsp;"
      (let ((status (aggregated-status (getf lib-test-result :status))))
        (format nil "<a class=\"test-status ~A\" href=\"~A\">~A</a>"
                (status-css-class status)
                (lib-log-uri lib-test-result)
                (single-letter-status status)))))

(defun test-runs-table-html (&optional
                             (db test-grid::*db*)
                             (status-renderer 'render-single-letter-status))
  (with-output-to-string (out)
    (write-line "<table cellspacing=\"1\" class=\"tablesorter\">" out)

    (princ "<thead><tr style=\"vertical-align: bottom;\"><th>Start Time</th><th>Lib World</th><th>Lisp</th><th>Runner</th>" out)
    (dolist (lib test-grid::*all-libs*)
      (format out "<th>~A</th>" (vertical-html lib)))
    (write-line "</tr></thead>" out)

    (write-line "<tbody>" out)
    (dolist (run (getf db :runs))
      (let ((run-descr (test-grid::run-descr run))
            (lib-statuses (test-grid::run-results run)))
        (format out "<tr><td>~A</td><td>~A</td><td>~A</td><td>~A</td>"
                (test-grid::pretty-fmt-time (getf run-descr :time))
                (getf run-descr :lib-world)
                (getf run-descr :lisp)
                (getf (getf run-descr :contact) :email))
        (dolist (lib test-grid::*all-libs*)
          (format out "<td>~A</td>"
                  (funcall status-renderer run (find lib lib-statuses
                                                     :key (test-grid::getter :libname)))))
        (write-line "</tr>" out)))
    (write-line "</tbody>" out)
    (write-line "</table>" out)))

(defun test-runs-report (&optional (db test-grid::*db*))
  (fmt-template *test-runs-report-template*
                `(("{THE-TABLE}" . ,(test-runs-table-html db))
                  ("{TIME}" . ,(test-grid::pretty-fmt-time (get-universal-time))))))

;; ============== CSV export ==================
(defun export-to-csv (out &optional (db test-grid::*db*))
  (format out "Lib World,Lisp,Runner,LibName,Status,TestDuration~%")
  (dolist (run (getf db :runs))
    (let ((run-descr (test-grid::run-descr run)))
      (dolist (lib-result (test-grid::run-results run))
        (format out "~a,~a,~a,~a,~a,~a~%"
                (getf run-descr :lib-world)
                (getf run-descr :lisp)
                (getf (getf run-descr :contact) :email)
                (string-downcase (getf lib-result :libname))
                (aggregated-status (getf lib-result :status))
                (float (getf lib-result :test-duration)))))))

;; ========= Pivot Reports ==================

;; Conceptualy, test results are objects with various fields:
;; :libname, :lib-world, :lisp, :log-byte-size, :test-duration,
;; :log-byte-length, etc.
;;
;; To build pivot reports, we need to access the
;; results for the report table cells. Every table
;; cell is lying on the crossing of the table rows
;; and columns.
;;
;; In other words, we need to access test results for every
;; combination of values for the fields we put into rows
;; and columsn headers.
;;
;; Currently we want only 3 properties in the row or column
;; headers: :lib-world, :lisp, :libname.
;;
;; Let's build the index - a hash table where keys are
;; 3 element lists representing a particular combination
;; of lib-world, libname and lisp; and the hash table value is
;; a list of test results for that combination:
;;
;; ("sbcl-win-1" "quicklisp 1" "alexandria")     -> (<test results>)
;; ("sbcl-win-1" "quicklisp 1" "babel")          -> (<test results>)
;; ("sbcl-linux-1" "quicklisp 1" "alexandria")   -> (<test results>)
;; ("sbcl-linux-1" "quicklisp 2" "alexandria")   -> (<test results>)
;; ("clisp-win-1" "quicklisp 2" "flexi-streams") -> (<test results>)
;; ...
;;
(defun do-results-impl (db handler)
  "Handler is a function of two arguments: TEST-RUN and LIB-RESULT"
    (dolist (test-run (getf db :runs))
      (dolist (lib-result (test-grid::run-results test-run))
        (funcall handler test-run lib-result))))

(defmacro do-results ((test-run-var lib-result-var db) &body body)
  `(do-results-impl ,db
     #'(lambda (,test-run-var ,lib-result-var)
         ,@body)))

(defun build-joined-index (db)
  (let ((all-results (make-hash-table :test 'equal)))
    (do-results (run lib-result db)
      (let* ((run-descr (test-grid::run-descr run))
             (lisp (getf run-descr :lisp))
             (lib-world (getf run-descr :lib-world))
             (libname (getf lib-result :libname)))
        (push lib-result
              (gethash (list lisp lib-world libname) all-results))))
    all-results))

;; The pivot reports code below does not know exact form
;; of the index key - in what order lib-world, lisp and libname
;; values are specified in the key. Moreover, the pivot reports code
;; does not know we chosen only these 3 properties for the pivot table
;; headers - it receives the property names for row and column headers
;; as parameters. All what the pivot code below knows, is that the
;; index is a hash table where keys store field values somehow,
;; and that the hash tabble values are lists of results.
;;
;; To abstract away the index format we provide the following functions:

(defun make-index-key ()
  (make-sequence 'list 3))

(defun make-fields-values-setter (fields)
  "Returns a function accepting index key
as the first parameter, list of field values the second parameters,
and destructively modifies the index key by setting the field
values in it. Names of fields to modify in the index key,
and their order is specified by FIELDS."
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
  "Returns a function accepting and index key as a parameter,
and returning a list of field values from that index key.
The field names to retrieve, and their order in the
returned list is specified by FIELDS."
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

;; Lets introduce a notion of row and columns addresses.
;; An address is a list with values for the fields
;; we put into the row or column header.
;;
;; For example, if we want a privot report with lisp
;; implementations as columns, and lisp-world and
;; library name as rows, then we may have column
;; addresses like
;;   ("sbcl-win-1.0.55")
;;   ("clisp-win-2.49")
;;   ("abcl-1.1")
;; and row addresses like
;;   ("quickisp 2012-02-03" "alexandria")
;;   ("quickisp 2012-02-03" "babel")
;;   ("quickisp 2012-03-03" "cl-json")
;;
;; Order of values in the address depends on the order
;; of grouping we chosen for pivot reports. In the above
;; row addresses lib-worlds are larger groups, and for
;; every lib-world we enumerate library names. In another
;; pivot report we may want other way around: to first group
;; data by libnames, and then subdivide these groups by
;; lib-worlds. In this case row addresses would be
;;   ("alexandria" "quickisp 2011-12-07" )
;;   ("alexandria" "quickisp 2012-01-03" )
;;   ("alexandria" "quickisp 2012-02-03" )
;;   ("babel" "quickisp 2011-12-07" )
;;   ("babel" "quickisp 2012-02-03" )
;;

(defun distinct-addresses (joined-index address-fields)
  (let ((distinct (make-hash-table :test #'equal))
        (fields-getter (make-fields-values-getter address-fields)))
    (maphash #'(lambda (index-key unused-index-value)
                 (declare (ignore unused-index-value))
                 (setf (gethash (funcall fields-getter index-key)
                                distinct)
                       t))
             joined-index)
    (test-grid::hash-table-keys distinct)))

;; Take into account the specifics of HTML tables - the
;; headers which group several rows or columns, will
;; have a single table cell with rowspan or colspan atribute
;; set to the number of rows/columns in the group.
;;
;; Example HTML for a pivot report with row headers
;; containing lib-world and libname (in that order),
;; and column headers containing lisp implementation name.
;; Pay attantion to the rowspan attribute:

#|

<tr> <th>                                </th> <th>          </th> <th>sbcl-win-1.0.55</th> <th>clisp-linux-2.49</th> </tr>
<tr> <th rowspan="3">quicklisp 2012-01-03</th> <th>alexandria</th> <td>    data...    </td> <td>    data...     </td> </tr>
<tr>                                           <th>babel     </th> <td>    data...    </td> <td>    data...     </td> </tr>
<tr>                                           <th>cl-json   </th> <td>    data...    </td> <td>    data...     </td> </tr>
<tr> <th rowspan="2">quicklisp 2012-02-07</th> <th>alexandria</th> <td>    data...    </td> <td>    data...     </td> </tr>
<tr>                                           <th>cl-json   </th> <td>    data...    </td> <td>    data...     </td> </tr>

|#

;; If we put more than one field into the columns, we whould
;; have similar situation with colspans.
;;
;; When printing the table row by row, cell by cell, we need to know
;; what will be rowspan or colspan for particular <th> cell,
;; and whether the row we are currently printing should have the 
;; <th rowspan="Y"> element, or the TH was already printed in a
;; previous row in the same group. (Similar for <th colspan="X"> 
;; elements in the rolumn headers).
;;
;; Lets precalculate some usefull information, which will allow
;; us to make the correct decision.
;;
;; A helper function:
(defun subaddrs (row-address)
  "Subaddress is a prefix of row or column address.
Every subaddress represents some level of pivot groupping."
  (nreverse (maplist #'reverse (reverse row-address))))

(assert (equal '((1) (1 2) (1 2 3))
               (subaddrs '(1 2 3))))

;; Note, that every address of length N has
;; N subaddresses. 
;; E.g. (length (subaddrs '("x" "y" "z"))) == 3.
;;
;; And note also that every subadderss is a column 
;; in a row header, or a row in a column header.
;; 
;; [     ql  ]
;; [     lisp]
;; [ lib     ]
;;
;; Here row addresses contain ony one component -
;; lib name: ("alexandria"), ("babel"), etc.,
;; and row headers need only one column.
;;
;; Column addresses contains two components:
;; quicklisp name and lisp name: 
;; ("quicklisp 2012-01-08" "ccl-1"),
;; ("quicklisp 2012-01-08" "clisp-2"), 
;; and the table column headers occupy two rows.

;; For every subaddress (group) we calculate it's span 
;; (number of rows/columns in the group),
;; and store a flag, whether we already printed
;; the <th>.
(defstruct (header-print-helper :conc-name)
  (span 0 :type fixnum)
  (printed nil))

;; Here is the main calculation. Returns
;; a hash table mapping subaddresses to header-print-helper objects.
;; For the above example of pivot HTML table, the hashtable calculated
;; for rows would be:
;;
;;  ("quicklisp 2012-01-03")              -> #S(span 3 printed nil)
;;  ("quicklisp 2012-01-03" "alexandria") -> #S(span 1 printed nil)
;;  ("quicklisp 2012-01-03" "cl-json")    -> #S(span 1 printed nil)
;;  ("quicklisp 2012-01-03" "cl-json")    -> #S(span 1 printed nil)
;;  ("quicklisp 2012-02-07")              -> #S(span 2 printed nil)
;;  ("quicklisp 2012-02-07" "alexandria") -> #S(span 1 printed nil)
;;  ("quicklisp 2012-02-07" "cl-json")    -> #S(span 1 printed nil)

(defun calc-spans (row-or-col-addrs)
  (let ((helpers (make-hash-table :test #'equal)))
    (dolist (row-or-col-addr row-or-col-addrs)
      (dolist (subaddr (subaddrs row-or-col-addr))
        (let ((helper (or (gethash subaddr helpers)
                          (setf (gethash subaddr helpers)
                                (make-header-print-helper)))))
          (incf (span helper)))))
    helpers))

;; This is how we print data in table data cells.
(defun format-lib-results (out lib-results)
  (dolist (lib-result lib-results)
    (let ((status (aggregated-status (getf lib-result :status))))
      (format out "<a href=\"~a\" class=\"test-status ~a\">~a</a> "
              (lib-log-uri lib-result)
              (status-css-class status)
              (string-downcase status)))))

(defun print-row-header (row-addr row-spans out)
  (dolist (subaddr (subaddrs row-addr))
    (let* ((helper (gethash subaddr row-spans))
           (rowspan (span helper))
           (maybe-css-class (if (> rowspan 1) "class=\"no-hover\"" "")))
      (when (not (printed helper))
        (format out "<th rowspan=\"~A\" ~A>~A</th>"
                rowspan maybe-css-class
                (string-downcase (car (last subaddr))))
        (setf (printed helper) t)))))

;; Two alternative ways of column headers printing.
;;
;; This is used when we have enough horizontal space:
;; just a usual <th> element.
(defun print-usual-col-header (colspan text out)
  (format out "<th colspan=\"~A\">~A</th>" colspan text))

;; When we need to save horizontal space, we print
;; column headers rotaget, so that headers do not
;; require more than 1 ex from columns width
;; (the data cell may make the column wider
;; than 1ex of course, but the headers never
;; increase the column width more than needed
;; for the data cells).
(defun print-rotated-col-header (colspan text out)
  ;; Calculate the height so that rotated text fits
  ;; into it: multiply the length of the text to
  ;; sine of the rotation (35 deegrees)
  ;; and add 3 ex for sure.
  (let ((css-height (+ (ceiling (* (sin (/ (* pi 35.0) 180.0))
                                   (length text)))
                       3)))
    (format out "<th colspan=\"~A\" class=\"rotated-header\" style=\"height: ~Aex\"><div>~A</div></th>"
            colspan css-height text)))

(defun print-col-headers (row-field-count col-field-count cols out)
  (let ((col-count (length cols))
        (col-spans (calc-spans cols))
        (header-row-count col-field-count))
    (dotimes (i (+ row-field-count col-count))
      (princ "<colgroup/>" out))
    (dotimes (header-row-num header-row-count)
      (princ "<tr class=\"header-row\">" out)
      (dotimes (row-header row-field-count)
        (princ "<th>&nbsp;</th>" out))
      (dolist (col-addr cols)
        (let* ((cell-addr (subseq col-addr 0 (1+ header-row-num)))
               (helper (gethash cell-addr col-spans)))
          (when (not (printed helper))
            (let ((colspan (span helper))
                  (text (string-downcase (car (last cell-addr)))))
              ;; Heuristic to determine if we need to rotate
              ;; the column headers: more than 7 columns, and it is the
              ;; last row of column headers.
              ;;
              ;; (BTW, this is the place where the code might need to
              ;; be adjusted whould we start using it for pivot reports
              ;; by more than the 3 fields - :lib-world, :libname, :lisp -
              ;; in the headers; actually; actually it's not perfect even
              ;; now)
              (if (and (> col-count 7)
                       (= header-row-num (1- header-row-count)))
                  (print-rotated-col-header colspan text out)
                  (print-usual-col-header colspan text out)))
            (setf (printed helper) t))))
      (format out "</tr>~%"))))

(defun pivot-table-html (out
                         joined-index
                         row-fields row-fields-sort-predicates
                         col-fields col-fields-sort-predicates)

  (assert (= (length row-fields) (length row-fields-sort-predicates)))
  (assert (= (length col-fields) (length col-fields-sort-predicates)))

  (princ "<table border=\"1\" class=test-table>" out)

  (let* ((row-comparator #'(lambda (rowa rowb)
                             (test-grid::list< row-fields-sort-predicates
                                               rowa rowb)))
         (col-comparator #'(lambda (cola colb)
                             (test-grid::list< col-fields-sort-predicates
                                               cola colb)))
         (rows (sort (distinct-addresses joined-index row-fields)
                     row-comparator))
         (row-spans (calc-spans rows))
         (cols (sort (distinct-addresses joined-index col-fields)
                     col-comparator))
         ;; this index key will be destructively modified
         ;; when we need to access the data, to avoid
         ;; consing of creation of new indexe every time
         ;; (is it a prelimenary optimization?)
         (index-key (make-index-key))
         (rows-fields-setter (make-fields-values-setter row-fields))
         (cols-fields-setter (make-fields-values-setter col-fields)))

    (print-col-headers (length row-fields) (length col-fields) cols out)
    (flet ((test-results-by-row-col (row-addr col-addr)
             (funcall rows-fields-setter index-key row-addr)
             (funcall cols-fields-setter index-key col-addr)
             (gethash index-key joined-index)))
      (dolist (row rows)
        (princ "<tr>" out)
        (print-row-header row row-spans out)
        (dolist (col cols)
          (let ((lib-results (test-results-by-row-col row col)))
            (princ "<td>" out)
            (format-lib-results out lib-results)
            (princ "</td>" out)))
        (format out "</tr>~%"))))
  (princ "</table>" out))

(defvar *pivot-report-template*
  (merge-pathnames "pivot-report-template.html"
                   (src-dir)))

(defun pivot-report-html (out
                          joined-index
                          row-fields row-fields-sort-predicates
                          col-fields col-fields-sort-predicates)

  (let ((table (with-output-to-string (str)
                 (pivot-table-html str
                                   joined-index
                                   row-fields row-fields-sort-predicates
                                   col-fields col-fields-sort-predicates))))

    (write-sequence (fmt-template *pivot-report-template*
                                  `(("{THE-TABLE}" . ,table)
                                    ("{TIME}" . ,(test-grid::pretty-fmt-time (get-universal-time)))))
                    out)))

(defun print-pivot-reports (joined-index)
  (flet ((print-report (filename
                        row-fields row-fields-sort-predicates
                        col-fields col-fields-sort-predicates)
           (with-report-file (out filename)
             (pivot-report-html out
                                joined-index
                                row-fields row-fields-sort-predicates
                                col-fields col-fields-sort-predicates))))

    (print-report "pivot_ql_lisp-lib.html"
                  '(:lib-world) (list #'string>)
                  '(:lisp :libname) (list #'string< #'string<))
    (print-report "pivot_ql_lib-lisp.html"
                  '(:lib-world) (list #'string>)
                  '(:libname :lisp) (list #'string< #'string<))

    (print-report "pivot_lisp_lib-ql.html"
                  '(:lisp) (list #'string<)
                  '(:libname :lib-world) (list #'string< #'string>))
    (print-report "pivot_lisp_ql-lib.html"
                  '(:lisp) (list #'string<)
                  '(:lib-world :libname) (list #'string> #'string<))

    (print-report "pivot_lib_lisp-ql.html"
                  '(:libname) (list #'string<)
                  '(:lisp :lib-world) (list #'string< #'string>))
    (print-report "pivot_lib_ql-lisp.html"
                  '(:libname) (list #'string<)
                  '(:lib-world :lisp) (list #'string> #'string<))

    (print-report "pivot_ql-lisp_lib.html"
                  '(:lib-world :lisp) (list #'string> #'string<)
                  '(:libname) (list #'string<))
    (print-report "pivot_ql-lib_lisp.html"
                  '(:lib-world :libname) (list #'string> #'string<)
                  '(:lisp) (list #'string<))

    (print-report "pivot_lisp-lib_ql.html"
                  '(:lisp :libname) (list #'string< #'string<)
                  '(:lib-world) (list #'string>))
    (print-report "pivot_lisp-ql_lib.html"
                  '(:lisp :lib-world) (list #'string< #'string>)
                  '(:libname) (list #'string<))

    (print-report "pivot_lib-lisp_ql.html"
                  '(:libname :lisp) (list #'string< #'string<)
                  '(:lib-world) (list #'string>))
    (print-report "pivot_lib-ql_lisp.html"
                  '(:libname :lib-world) (list #'string< #'string>)
                  '(:lisp) (list #'string<))))

;; ========= Regressions between quicklisp distro versions ==================

;;; Alternative representation of a library test status.
;;;
;;; Fail-list is an always sorted list of failures.
;;;
;;; Failures [elements of this list] are strings - failed test
;;; names; or conses in the form (:unexpected-ok . "test-name")
;;; - names of the tests which passed successfully despite
;;; they are known to fail.
;;;
;;; The sort order is so that unexpected OKs are always
;;; at the end.
;;;
;;; Example:
;;; ("test-a2" "test-b" (:unexpected-ok "test-a1"))
;;;
;;; Note that fail-list does not distinguish known failures from unknwown failures;
;;; all of them are represented just by stings naming failed tests.

;; Sort order for failures
(defun fail-less (fail-a fail-b)
  (if (stringp fail-a)
      (if (stringp fail-b)
          (string< fail-a fail-b)  ; "test-a" "test-b"
          t)                       ; "test-a" (:unexpected-ok . "test-b")
      (if (stringp fail-b)
          nil                      ; (:unexpected-ok . "test-a") "test-b"
          (string< (cdr fail-a)    ; (:unexpected-ok . "test-a") (:unexpected-ok . "test-b")
                   (cdr fail-b)))))

;; Helper function for fail-list creation:
(defun sort-fail-list (fail-list)
  "Destructively sort the FAIL-LIST"
  (sort fail-list #'fail-less))

(assert (equal '("a" "a2" "a3" "b" "b2" "b3" (:unexpected-ok . "a2.2") (:unexpected-ok . "b2.2"))
               (sort-fail-list '("a" (:unexpected-ok . "a2.2") "b3" "b" "a2" "a3" (:unexpected-ok . "b2.2") "b2"))))

;; Convert the extended test status from the db.lisp form:
;;    (:failed-tests ("test-a" "test-b") :known-to-fail ("test-c"))
;; to the fail-list form:
;;    ("test-a" "test-b" (:unexpected-ok . "test-c"))
(defun fail-list (lib-status)
  (assert (listp lib-status))
  (let* ((failures (getf lib-status :failed-tests))
         (unexpected-oks (set-difference (getf lib-status :known-to-fail)
                                         failures
                                         :test #'string=)))
    (sort-fail-list (append failures
                            (mapcar #'(lambda (unexpected-ok-test) (cons :unexpected-ok unexpected-ok-test))
                                    unexpected-oks)))))
(assert
 (and (equal '() (fail-list '(:failed-tests ())))
      (equal '("a" "b") (fail-list '(:failed-tests ("a" "b"))))
      (equal '("a" "b") (fail-list '(:failed-tests ("b" "a"))))
      (equal '("a" "b" (:unexpected-ok . "c"))
             (fail-list '(:failed-tests ("a" "b") :known-to-fail ("b" "c"))))))

(defparameter *lib-status-regressions-rules*
  `( ;new type             ;old type     ; do the new-result has regressions comparing to old-status?
    (()                     t            nil)
    (:ok                    t            nil)
    (:no-resource           t            nil)
    (:fail                  ()           t)
    (:fail                  :ok          t)
    (:fail                  (something)  nil)
    (:fail                  :fail        nil)
    (:fail                  :no-resource nil)
    ;; (unexpected-oks-only) is a subtype of (something)
    ;; therefore should be checked before (something)
    ((unexpected-oks-only)  :ok          nil)
    ((something)            ()           t)
    ((something)            :ok          t)
    ((something)            :no-resource nil)
    ((something)            (something)  ,#'(lambda (new-status old-status)
                                              (set-difference (fail-list new-status)
                                                              (fail-list old-status)
                                                              :test #'equal)))
    ((something)            :fail        nil)))

(defun of-type-p (lib-status lib-status-typespec)
  (cond ((eq lib-status-typespec t)
         t)
        ((member lib-status-typespec '(:ok :fail :no-resource))
         (eq lib-status lib-status-typespec))
        ((equal lib-status-typespec '())
         (and (listp lib-status)
              (not (or (getf lib-status :failed-tests)
                       (getf lib-status :known-to-fail)))))
        ((equal lib-status-typespec '(something))
         (and (listp lib-status)
              (or (getf lib-status :failed-tests)
                  (getf lib-status :known-to-fail))))
        ((equal lib-status-typespec '(unexpected-oks-only))
         (and (consp lib-status)
              (let ((failed (getf lib-status :failed-tests))
                    (known-to-fail (getf lib-status :known-to-fail)))
                (and (null failed)
                     (car known-to-fail)))))
        (t (error "Unknown lib-status-typespec: ~S" lib-status-typespec))))

(assert
 (and (of-type-p '(:failed-tests ("a")) '(something))
      (of-type-p '(:failed-tests ("a" "b") :known-to-fail ("a")) '(something))
      (of-type-p '() '())
      (not (of-type-p '(:failed-tests "a") '()))
      (of-type-p :fail :fail)
      (of-type-p :no-resource :no-resource)
      (of-type-p :ok :ok)
      (of-type-p :ok t)
      (not (of-type-p '() '(unexpected-oks-only)))
      (of-type-p '(:failed-tests () :known-to-fail ("a" "b")) '(unexpected-oks-only))
      (of-type-p '(:failed-tests () :known-to-fail ("a" "b")) '(something))
      (of-type-p '(:failed-tests () :known-to-fail ("a" "b")) t)))

(defun has-regressions-p (new-lib-status old-lib-status)
  "Returns true if NEW-LIB-STATUS has regressions comparing to OLD-LIB-STATUS.
In most cases the result is obvious, but there is one subtle case, which deserves to be 
explained here: if NEW-LIB-STATUS contains only unexpected OKs, and OLD-LIB-STATUS
was :OK, then HAS-REGRESSIONS-P returns false - that's how we treat :OK, even
unexpected OKs are OKs."
  (loop for (new-typespec old-typespec result-spec) in *lib-status-regressions-rules*
     do (when (and (of-type-p new-lib-status new-typespec)
                   (of-type-p old-lib-status old-typespec))
          (return-from has-regressions-p
            (if (typep result-spec 'function)
                (funcall result-spec new-lib-status old-lib-status)
                result-spec))))
  (error "Unrecognized lib-status combination. new-lib-status: ~S, old-lib-status: ~S"
         new-lib-status old-lib-status))

(assert
 (and (not (has-regressions-p :ok :fail))
      (not (has-regressions-p :ok '(:failed-tests ("a"))))
      (not (has-regressions-p :no-resource :fail))
      (not (has-regressions-p :no-resource :ok))
      (not (has-regressions-p :no-resource :no-resource))
      (not (has-regressions-p '(:failed-tests () :known-to-fail ()) :ok))
      (not (has-regressions-p '(:failed-tests () :known-to-fail ()) :fail))
      (not (has-regressions-p '(:failed-tests () :known-to-fail ()) '(:failed-tests ("a"))))
      (has-regressions-p '(:failed-tests ("a" "b")) '(:failed-tests ("c")))
      (not (has-regressions-p '(:failed-tests ("a" "b")) '(:failed-tests ("a" "b"))))
      (has-regressions-p '(:failed-tests ("a" "b")) '())
      (has-regressions-p '(:failed-tests ("a" "b")) :ok)
      ;; unexpected OKs only - is not a regression comparing to :OK
      (not (has-regressions-p '(:failed-tests () :known-to-fail ("a" "b"))
                              :ok))
      ;; but is a regression comparing an extended status with no failures
      (has-regressions-p '(:failed-tests () :known-to-fail ("a" "b"))
                         '(:failed-tests () :known-to-fail ()))
      (has-regressions-p :fail :ok)
      (has-regressions-p :fail '(:failed-tests () :known-to-fail ()))
      (not (has-regressions-p '(:failed-tests () :known-to-fail ()) :fail))
      (not (has-regressions-p :ok '(:failed-tests () :known-to-fail ())))
      (not (has-regressions-p :fail :fail))))

;; Diff item represent two results
;; of the same library under the same lisp,
;; but in different versions of quicklisp disto.
(defclass quicklisp-diff-item ()
  ((libname :initarg :libname :accessor libname)
   (lisp :initarg :lisp :accessor lisp)
   (new-status :initarg :new-status :accessor new-status)
   (old-status :initarg :old-status :accessor old-status)))

;; Diff of two quicklisp distro versions.
(defclass quicklisp-diff ()
  (;; diff-items where new quicklisp distro version has regressions
   ;; comparing to old version (note, at the same time
   ;; it may have improvements.
   ;;
   ;; For example previously test-a and test-b
   ;; failed, but in the new version test-a and test-c fail.
   ;; test-c is a regression - it fails now but not previously;
   ;; test-b is an improvement - it failed previously but not now;
   ;;
   ;; Independently on improvements presense, if the library
   ;; has regressiosn in new version, the quicklisp-diff-item
   ;; is put into the have-regressions list.
   (have-regressions :type list :initform '() :accessor have-regressions)
   ;; Here we put quicklisp-diff-itmes for libraries
   ;; which have only improvements in the new quicklisp distro
   ;; version.
   (imrovements-only :type list :initform '() :accessor improvements-only)))

(defun compare-quicklisps (db-index quicklisp-new quicklisp-old)
  "Returns QUICKLISP-DIFF for the two quicklisp distro versions
specified by QUICKLISP-NEW and QUICKLISP-OLD."
  (let* ((diff (make-instance 'quicklisp-diff))
         (lib-world-getter* (make-fields-values-getter '(:lib-world)))
         (lib-world-setter* (make-fields-values-setter '(:lib-world)))
         (lisp-getter* (make-fields-values-getter '(:lisp)))
         ;; todo: use alexandria:compose
         (lib-world-getter (lambda (index-key) (car (funcall lib-world-getter* index-key))))
         (lib-world-setter (lambda (index-key lib-world)
                             (funcall lib-world-setter* index-key (list lib-world))))
         (lisp-getter (lambda (index-key) (car (funcall lisp-getter* index-key))))
         (new-quicklisp-keys (remove quicklisp-new
                                     (test-grid::hash-table-keys db-index)
                                     :key lib-world-getter
                                     :test (complement #'string=))))
    (dolist (key new-quicklisp-keys)
      (let ((key-prev (copy-list key)))
        (funcall lib-world-setter key-prev quicklisp-old)
        (let ((results (gethash key db-index))
              (results-prev (gethash key-prev db-index)))

          ;; order our results-diff report by library name
          (setf results (sort (copy-list results)
                              (lambda (lib-result-a lib-result-b)
                                (string< (getf lib-result-a :libname)
                                         (getf lib-result-b :libname)))))

          (dolist (lib-result results)
            (let ((status (getf lib-result :status)))
              (dolist (lib-result-prev results-prev)
                (let ((status-prev (getf lib-result-prev :status)))
                  (flet ((make-diff-item ()
                           (make-instance 'quicklisp-diff-item
                                       :libname (getf lib-result :libname)
                                       :lisp (funcall lisp-getter key)
                                       :new-status status
                                       :old-status status-prev)))
                    (cond ((has-regressions-p status status-prev)
                           (push (make-diff-item)
                                 (have-regressions diff)))
                          ((has-regressions-p status-prev status)
                           (push (make-diff-item)
                                 (improvements-only diff))))))))))))
    diff))

(defun print-quicklisp-diff (destination ql-new ql-old quicklisp-diff)
  (flet ((print-diff-item (diff-item)
           (let ((*print-pretty* nil))
             (format destination "~a, ~a:~%~a: ~a~%~a: ~a~%~%"
                     (string-downcase (libname diff-item))
                     (lisp diff-item)
                     ql-new
                     (new-status diff-item)
                     ql-old
                     (old-status diff-item)))))
    (format destination "~%~%***************************************************************************~%")
    (format destination "* test results diff between ~A and ~A *~%" ql-new ql-old)
    (format destination "***************************************************************************~%~%")
    (format destination "************* Have Regressions *************~%")
    (dolist (diff-item (have-regressions quicklisp-diff))
      (print-diff-item diff-item))
    (format destination "************* Improvements Only *************~%")
    (dolist (diff-item (improvements-only quicklisp-diff))
      (print-diff-item diff-item))))

(defun print-all-quicklisps-diff-report (destination joined-index)
  (let ((quicklisps (mapcar #'car (distinct-addresses joined-index '(:lib-world)))))
    (loop
       for qls on (sort quicklisps #'string>)
       do (let ((ql-new (first qls))
                (ql-old (second qls)))
            (when ql-old ;; not reached the end of list yet
              (print-quicklisp-diff destination
                                    ql-new
                                    ql-old
                                    (compare-quicklisps joined-index ql-new ql-old)))))))

;; =========== print all the reports at once =============

(defun generate-reports (&optional (db test-grid::*db*))

  (with-report-file (out "test-runs-report.html")
    (write-sequence (test-runs-report db) out))

  (with-report-file (out "export.csv")
    (export-to-csv out))

  (let ((joined-index (build-joined-index db)))

    (print-pivot-reports joined-index)

    (with-report-file (out "quicklisps-test-diff.txt")
      (print-all-quicklisps-diff-report out joined-index))))

