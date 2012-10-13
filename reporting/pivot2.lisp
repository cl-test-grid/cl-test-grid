;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

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
