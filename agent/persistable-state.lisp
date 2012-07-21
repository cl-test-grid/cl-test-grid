;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; Persistable agent state. Readable/writtable by
;;;; standard lisp functions. Keeps track which libraries this agent
;;;; have already tested on particular lib-worlds and
;;;; lisp implementations.

(defpackage #:test-grid-agent-state
  (:use #:cl)
  (:nicknames #:agent-state)
  (:export #:make-state
           #:done-p
           #:mark-done))

(in-package #:test-grid-agent-state)

#|

All the information is stored as a list of records;
each record is a list 3 elements: (<lib-world> <lisp> <library>)

After all the libraries known for test-grid
have been tested on a given lib-world and lisp,
we remove all the separate records for individual libraries,
and store one record in the form (<lib-world> <lisp> :done)

The same for lisp implementations: after all the lisp
implementations on the given lib-world are tested with
all the libraries, we store single record
in the form (<lib-world> :done :done)

Example of the agent state:

  (("quicklisp 2012-04-01" :done :done)
   ("quicklisp 2012-05-01" "clisp-2.49-win" :done)
   ("quicklisp 2012-05-01" "sbcl-1-win" :alexandria) 
   ("quicklisp 2012-05-01" "sbcl-1-win" :babel))

This means that all the lisps known for this agent and 
all the libraries are done already for lib-world named
"quicklisp 2012-04-05".

For "quicklisp 2012-05-05" we have tested all the libraries
on clisp, while for sbcl we've only tested alexandria and babel.

This :DONE feature not only compresses the repsentation
and makes it more readable; we also don't want to return
to old quicklisp distors after we tested them, even
if we add more libraries to test-grid later, which were not
tested on the old quicklisps. This mode is simpler for us
while we are tuning test-grid; we jsut run tests on the latest
quicklisp as it is out, and never go back in time even if
new libraries are added. Later we will probably switch to a
more flexible mode and collect test results for newly added libraries
from old quicklisp distros).

|#

(defun make-state ()
  '())

;;; Comparator functions for elements in the state records. 
;;;
;;; Consider two records:
;;;
;;;   ("quicklisp 2012-05-01" "clisp-2.49-win" :alexandria) 
;;;   ("quicklisp 2012-05-01" "clisp-2.49-win" :done)
;;;
;;; Here :DONE represents a "set of all the known libraries",
;;; and :ALEXANDRIA represents a single library; lets consider
;;; it as a set of single element.
;;; 
;;; We compare the elements as sets.

;; is element X a superset of Y ?
(defun elem>= (elem-x elem-y)
  (or (eq elem-x :done)
      (equal elem-x elem-y)))

;;; tests
(assert (elem>= "a" "a"))
(assert (not (elem>= "a" :done)))
(assert (elem>= :done :done))
(assert (elem>= :done "a"))

;; is element X a subset of Y ?
(defun elem<= (elem-x elem-y)
  (or (eq elem-y :done)
      (equal elem-x elem-y)))

;;; tests
(assert (elem<= "a" "a"))
(assert (elem<= "a" :done))
(assert (elem<= :done :done))
(assert (not (elem<= :done "a")))

;;; Comparators for the state records. Records are
;;; considered as specifications of sets of tested configurations;
(defun rec<= (x y)
  (tree-equal x y :test #'elem<=))

(defun rec>= (x y)
  (tree-equal x y :test #'elem>=))

;;; tests
(assert (rec<= '("quicklisp 2012-03-01" "clisp-2.49-win" :alexandria)
               '("quicklisp 2012-03-01" :done :done)))

(assert (not (rec<= '("quicklisp 2012-03-01" :done :done)
                    '("quicklisp 2012-03-01" "clisp-2.49-win" :alexandria))))

(assert (rec<= '("quicklisp 2012-03-01" :done :done)
               '("quicklisp 2012-03-01" :done :done)))

(assert (rec<= '("quicklisp 2012-03-01" "clisp-2.49-win" :alexandria)
               '("quicklisp 2012-03-01" "clisp-2.49-win" :alexandria)))

(assert (rec>= '("quicklisp 2012-03-01" :done :done)
               '("quicklisp 2012-03-01" "clisp-2.49-win" :alexandria)))

(assert (not (rec>= '("quicklisp 2012-03-01" "clisp-2.49-win" :alexandria)
                    '("quicklisp 2012-03-01" :done :done))))

(assert (rec>= '("quicklisp 2012-03-01" :done :done)
               '("quicklisp 2012-03-01" :done :done)))

(assert (rec>= '("quicklisp 2012-03-01" "clisp-2.49-win" :alexandria)
               '("quicklisp 2012-03-01" "clisp-2.49-win" :alexandria)))

(defun done-p (state lib-world &optional lisp library)
  (find (list lib-world (or lisp :done) (or library :done))
        state
        :test #'rec<=))

;; tests
(let ((state '(("quicklisp 2012-04-01" :done :done)
               ("quicklisp 2012-05-01" "clisp-2.49-win" :done)
               ("quicklisp 2012-05-01" "sbcl-1-win" :alexandria)
               ("quicklisp 2012-05-01" "sbcl-1-win" :babel))))
  (assert (done-p state "quicklisp 2012-04-01"))
  (assert (not (done-p state "quicklisp 2012-05-01")))
  (assert (done-p state "quicklisp 2012-05-01" "clisp-2.49-win"))
  (assert (done-p state "quicklisp 2012-05-01" "clisp-2.49-win" :alexandria))
  (assert (not (done-p state "quicklisp 2012-05-01" "sbcl-1-win")))
  (assert (done-p state "quicklisp 2012-05-01" "sbcl-1-win" :alexandria)))

(defun mark-done (state quicklisp &optional lisp library)
  "Caution! Returns new state destructively build from the old STATE."
  (let* ((new-record (list quicklisp (or lisp :done) (or library :done)))
         ;; first delete all records specifying subsets of what
         ;; is specified by the new-record
         (cleared-state (delete new-record state :test #'rec>=)))
    ;; and add the new-record to the state
    (cons new-record cleared-state)))

;; tests
(let ((state nil))
  (setf state (mark-done state "quicklisp 2012-05-01" "sbcl-1-win" :alexandria))
  (assert (done-p state "quicklisp 2012-05-01" "sbcl-1-win" :alexandria))
  (assert (not (done-p state "quicklisp 2012-05-01" "sbcl-1-win")))
  (assert (not (done-p state "quicklisp 2012-05-01")))
  (assert (equal state '(("quicklisp 2012-05-01" "sbcl-1-win" :alexandria))))

  (setf state (mark-done state "quicklisp 2012-05-01" "sbcl-1-win"))
  (assert (done-p state "quicklisp 2012-05-01" "sbcl-1-win" :alexandria))
  (assert (done-p state "quicklisp 2012-05-01" "sbcl-1-win" :babel))
  (assert (done-p state "quicklisp 2012-05-01" "sbcl-1-win"))
  (assert (not (done-p state "quicklisp 2012-05-01")))
  (assert (equal state '(("quicklisp 2012-05-01" "sbcl-1-win" :done))))

  (setf state (mark-done state "quicklisp 2012-05-01"))
  (assert (done-p state "quicklisp 2012-05-01" "sbcl-1-win" :alexandria))
  (assert (done-p state "quicklisp 2012-05-01" "sbcl-1-win" :babel))
  (assert (done-p state "quicklisp 2012-05-01" "sbcl-1-win"))
  (assert (done-p state "quicklisp 2012-05-01"))
  (assert (equal state '(("quicklisp 2012-05-01" :done :done)))))
