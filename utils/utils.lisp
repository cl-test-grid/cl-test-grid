;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: test-grid; Base: 10; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

(defpackage #:test-grid-utils
  (:nicknames :tg-utils)
  (:use :cl)
  (:export #:set=))

(in-package #:test-grid-utils)

(defun set= (set-a set-b &key (test #'eql) key)
  (null (set-exclusive-or set-a set-b :test test :key key)))

(defun starts-with (str prefix &key (test #'char=))
  (let ((mismatch (mismatch str prefix :test test)))
    (or (null mismatch)
        (>= mismatch (length prefix)))))

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

(defun plist-getter (prop)
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
  (with-standard-io-syntax
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (pprint obj out)))
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

;;; split a list into sublists by n elements,
;;; e.g. (a b c d e) by 3 => (a b c) (d e)
(defclass list-splitter ()
  ((remainder :type list
              :accessor remainder
              :initarg :list
              :initform (error ":list is required"))))

(defun next (list-splitter &optional (n 1))
  (let ((result nil))
    (dotimes (i n)
      (when (null (remainder list-splitter))
        (return-from next (nreverse result)))
      (push (car (remainder list-splitter)) result)
      (setf (remainder list-splitter)
            (cdr (remainder list-splitter))))
    (nreverse result)))

(let ((ls (make-instance 'list-splitter :list '(1 2 3))))
  (assert (equal '(1 2) (next ls 2)))
  (assert (equal '(3) (next ls 2)))
  (assert (equal nil (next ls 2))))

(defun split-list (list n)
  (let ((splitter (make-instance 'list-splitter :list list))
        (result nil))
    (loop
       (let ((sub (next splitter n)))
         (when (null sub)
           (return (nreverse result)))
         (push sub result)))))

(assert (equal '((1 2 3) (4 5))
               (split-list '(1 2 3 4 5) 3)))

