;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

(defun list-props (object prop-readers)
  (mapcar (lambda (accessor)
            (funcall accessor object))
          prop-readers))

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

(defun distinct (objects object-prop-readers)
  (remove-duplicates (mapcar (lambda (object) (list-props object object-prop-readers))
                             objects)
                     :test #'equal))

(assert (alexandria:set-equal '((1) (7))
                              (distinct '((:a 1 :b 2) (:a 1 :b 3) (:a 7 :b 1))
                                        (list (test-grid-utils::plist-getter :a)))                              
                              :test #'equal))



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

;;;; --- performance experiments

(defparameter *long-list* (reduce 'append (make-sequence 'list 10000
                                                         :initial-element '((:a 1 :b 2) (:a 1 :b 3) (:a 7 :b 1)))))

(defparameter *long-list* (map-into (make-sequence 'list (* 2000 100 100))
                                    (lambda ()
                                      (list :libname (random 2000) :lisp (random 100) :lib-world (random 100)))))

(/ 320000128.0 (length *long-list*))
(asdf:implementation-identifier)
(first *long-list*)
(time
 (defparameter *filtered-list* (remove-if-not (lambda (item)
                                                (member (getf item :lib-world) '(10 11 12)))
                                              *long-list*)))

(time
 (defparameter *filtered-list* (remove-if-not (constantly nil) *long-list*)))

(length *filtered-list*)

(time 
 (defparameter *distinct-list* (distinct *filtered-list* (list (test-grid-utils::plist-getter :libname)
                                                               (test-grid-utils::plist-getter :lib-world)))))
      
