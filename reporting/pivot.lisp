;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

;;; Pivot is essentially a grouping of items into cells.
;;; Groupping is done by values of properties chosen for rows
;;; and columns. Note that rotation of a pivot - i.e. moving
;;; a field from rows to columns doesn't affect the grouping,
;;; it is the same cells rotated.

(defun group-by (items item-prop-readers)
  (let ((h (make-hash-table :test #'equal)))
    (dolist (item items)
      (let ((key (list-props item item-prop-readers)))
        (push item (gethash key h))))
    h))

(let ((groups (group-by '((:x 1 :y 2)
                          (:x 3 :y 2)
                          (:x 3 :y 7))
                        (list (test-grid-utils::plist-getter :x)))))
  (assert
   (alexandria:set-equal (gethash '(3) groups)
                         '((:x 3 :y 2)
                           (:x 3 :y 7))
                         :test #'equal))
  (assert
   (alexandria:set-equal (gethash '(1) groups)
                         '((:x 1 :y 2))
                         :test #'equal)))

(let ((groups (group-by '((:x 1 :y 2)
                          (:x 5 :y 8)
                          (:x 5 :y 8)
                          (:x 6 :y 9))
                        (list
                         (test-grid-utils::plist-getter :x)
                         (test-grid-utils::plist-getter :y)))))
  (assert
   (equal (gethash '(1 2) groups)
          '((:x 1 :y 2))))

  (assert
   (alexandria:set-equal (gethash '(5 8) groups)
                         '((:x 5 :y 8)
                           (:x 5 :y 8))
                         :test #'equal))
  (assert
   (equal (gethash '(6 9) groups)
          '((:x 6 :y 9)))))


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
;; We use a notion of row and columns addresses.
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

(defun print-row-header (row-addr row-spans out)
  (dolist (subaddr (subaddrs row-addr))
    (let* ((helper (gethash subaddr row-spans))
           (rowspan (if (> (span helper) 7)
                        ;; Don't group too many rows under one row header
                        ;; because it's difficult to find the grouping value.
                        ;; Id it's than 7 rows are under this subaddr, pring separate
                        ;; header for each row.
                        1
                        (span helper)))
           (maybe-css-class (if (> rowspan 1) "class=\"no-hover\"" "")))
      (when (or (= rowspan 1)
                (not (printed helper)))
        (format out "<th rowspan=\"~A\" ~A>~A</th>"
                rowspan maybe-css-class
                (html-template:escape-string-all (string-downcase (car (last subaddr)))))
        (setf (printed helper) t)))))

;; Two alternative ways of column headers printing.
;;
;; This is used when we have enough horizontal space:
;; just a usual <th> element.
(defun print-usual-col-header (colspan text out)
  (format out "<th colspan=\"~A\">~A</th>"
          colspan
          (html-template:escape-string-all text)))

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
                  (text (format nil "~(~A~)" (car (last cell-addr)))))
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

;;; Several versions of pivot-table-html, different in how the
;;; the parameters are specified.

(defun pivot-table-html2 (out
                          objects
                          row-field-getters row-fields-sort-predicates
                          col-field-getters col-fields-sort-predicates
                          cell-formatter)

  (assert (= (length row-field-getters) (length row-fields-sort-predicates)))
  (assert (= (length col-field-getters) (length col-fields-sort-predicates)))

  (princ "<table border=\"1\" class=test-table>" out)

  (let* ((row-comparator #'(lambda (rowa rowb)
                             (test-grid-utils::list< row-fields-sort-predicates
                                               rowa rowb)))
         (col-comparator #'(lambda (cola colb)
                             (test-grid-utils::list< col-fields-sort-predicates
                                               cola colb)))
         (rows (sort (distinct objects row-field-getters)
                     row-comparator))
         (row-spans (calc-spans rows))
         (cols (sort (distinct objects col-field-getters)
                     col-comparator))
         (cells (group-by objects (append row-field-getters col-field-getters))))

    (print-col-headers (length row-field-getters) (length col-field-getters) cols out)
    (flet ((cell-objects (row-addr col-addr)
             (gethash (append row-addr col-addr) cells)))
      (dolist (row rows)
        (princ "<tr>" out)
        (print-row-header row row-spans out)
        (dolist (col cols)
          (princ "<td>" out)
          (funcall cell-formatter out (cell-objects row col))
          (princ "</td>" out))
        (format out "</tr>~%"))))
  (princ "</table>" out))

(defun pivot-table-html3 (out objects &key rows cols cell-printer)
  "Another version of PIVOT-TABLE-HTML, more convenient parameters."
  (assert (every (lambda (r) (and (first r) (second r)))
                 rows)
          nil
          "ROWS elements must have two elements: accessor function and sorting predicate")
  (assert (every (lambda (c) (and (first c) (second c)))
                 cols)
          nil
          "COLS elements must have two elements: accessor function and sorting predicate")
  (let ((row-field-getters (mapcar (lambda (x) (coerce (first x) 'function)) rows))
        (row-fields-sort-predicates (mapcar #'second rows))
        (col-field-getters (mapcar (lambda (x) (coerce (first x) 'function)) cols))
        (col-fields-sort-predicates (mapcar #'second cols)))
    (pivot-table-html2 out
                       objects
                       row-field-getters row-fields-sort-predicates
                       col-field-getters col-fields-sort-predicates
                       cell-printer)))

(defun pivot-table-html4 (objects &key rows cols cell-printer)
  "Another version of PIVOT-TABLE-HTML, now a function - returns STRING"
  (with-output-to-string (s)
    (pivot-table-html3 s objects :rows rows :cols cols :cell-printer cell-printer)))

(defun pivot-report (out pivot-table reports-root-dir-relative-path)
  (let ((html-template:*string-modifier* #'cl:identity))
    (html-template:fill-and-print-template (src-file "pivot-report-template.html")
                                           (list :the-table pivot-table
                                                 :time (test-grid-agent::pretty-fmt-time (get-universal-time))
                                                 :reports-root-dir-relative-path reports-root-dir-relative-path)
                                           :stream out)))

(defun print-pivot (file-name
                    objects
                    &key rows cols cell-printer)
  (with-report-file (out file-name)
    (pivot-report out (pivot-table-html4 objects
                                         :rows rows
                                         :cols cols
                                         :cell-printer cell-printer)
                  (reports-root-dir-relative-path file-name))))

(defun print-old-pivots (db quicklisps)
  (let* ((results (list-lib-results db
                                    :where (lambda (lib-result)
                                             (member (lib-world lib-result)
                                                     quicklisps
                                                     :test #'string=)))))
    (print-pivot "pivot_ql_lisp-lib.html"
                  results
                  :rows '((lib-world string>))
                  :cols '((lisp string<) (libname string<))
                  :cell-printer #'format-lib-results)
    (print-pivot "pivot_ql_lib-lisp.html"
                  results
                  :rows '((lib-world string>))
                  :cols '((libname string<) (lisp string<))
                  :cell-printer #'format-lib-results)

    (print-pivot "pivot_lisp_lib-ql.html"
                  results
                  :rows '((lisp string<))
                  :cols '((libname string<) (lib-world string>))
                  :cell-printer #'format-lib-results)
    (print-pivot "pivot_lisp_ql-lib.html"
                  results
                  :rows '((lisp string<))
                  :cols '((lib-world string>) (libname string<))
                  :cell-printer #'format-lib-results)

    (print-pivot "pivot_lib_lisp-ql.html"
                  results
                  :rows '((libname string<))
                  :cols '((lisp string<) (lib-world string>))
                  :cell-printer #'format-lib-results)
    (print-pivot "pivot_lib_ql-lisp.html"
                  results
                  :rows '((libname string<))
                  :cols '((lib-world string>) (lisp string<))
                  :cell-printer #'format-lib-results)

    (print-pivot "pivot_ql-lisp_lib.html"
                  results
                  :rows '((lib-world string>) (lisp string<))
                  :cols '((libname string<))
                  :cell-printer #'format-lib-results)
    (print-pivot "pivot_ql-lib_lisp.html"
                  results
                  :rows '((lib-world string>) (libname string<))
                  :cols '((lisp string<))
                  :cell-printer #'format-lib-results)

    (print-pivot "pivot_lisp-lib_ql.html"
                  results
                  :rows '((lisp string<) (libname string<))
                  :cols '((lib-world string>))
                  :cell-printer #'format-lib-results)
    (print-pivot "pivot_lisp-ql_lib.html"
                  results
                  :rows '((lisp string<) (lib-world string>))
                  :cols '((libname string<))
                  :cell-printer #'format-lib-results)

    (print-pivot "pivot_lib-lisp_ql.html"
                  results
                  :rows '((libname string<) (lisp string<))
                  :cols '((lib-world string>))
                  :cell-printer #'format-lib-results)
    (print-pivot "pivot_lib-ql_lisp.html"
                  results
                  :rows '((libname string<) (lib-world string>))
                  :cols '((lisp string<))
                  :cell-printer #'format-lib-results)))
