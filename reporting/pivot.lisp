;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

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

(defun build-joined-index (db &key where)
  (let ((all-results (make-hash-table :test 'equal)))
    (do-results (record db :where where)
      (push record
            (gethash (list (lisp record) (lib-world record) (libname record))
                     all-results)))
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
    (test-grid-utils::hash-table-keys distinct)))

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
(defun format-lib-results (out joined-lib-results)
  (dolist (joined-lib-result joined-lib-results)
    (let ((status (aggregated-status (getf (lib-result joined-lib-result) :status))))
      (format out "<a href=\"~a\" class=\"test-status ~a\">~a</a> "
              (lib-log-uri joined-lib-result)
              (status-css-class status)
              (single-letter-status status)))))

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
;; column headers rotated, so that headers do not
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
    (format out "<thead>~%")
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
              ;; in the headers; actually the solution isn't perfect even
              ;; for the current 3 fields case)
              (if (and (> col-count 7)
                       (= header-row-num (1- header-row-count)))
                  (print-rotated-col-header colspan text out)
                  (print-usual-col-header colspan text out)))
            (setf (printed helper) t))))
      (format out "</tr>~%"))
    (format out "</thead>~%")))

(defun pivot-table-html (out
                         joined-index
                         row-fields row-fields-sort-predicates
                         col-fields col-fields-sort-predicates)

  (assert (= (length row-fields) (length row-fields-sort-predicates)))
  (assert (= (length col-fields) (length col-fields-sort-predicates)))

  (princ "<table border=\"1\" class=test-table>" out)

  (let* ((row-comparator #'(lambda (rowa rowb)
                             (test-grid-utils::list< row-fields-sort-predicates
                                               rowa rowb)))
         (col-comparator #'(lambda (cola colb)
                             (test-grid-utils::list< col-fields-sort-predicates
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
                                    ("{TIME}" . ,(test-grid-agent::pretty-fmt-time (get-universal-time)))))
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
