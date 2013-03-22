;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; DEPENDENCY of a system S, is another system which must be
;;;;            loaded before S may be compiled, loaded
;;;;            and be fully operational.
;;;; DEPENDENT of a system S, is another side of the relation,
;;;;           a system which needs S to be loaded first.
;;;;
;;;; If X is a DEPENDENCY of Y, then Y is a DEPENDENT of X.

(in-package #:test-grid-reporting)

(defun reverse-dependencies (dependency-hash)
  "Accept a hash table from system name to a list of its depencendies,
and returns a hash table from system name to a list of its dependents."
  (let ((result (make-hash-table :test #'equal)))
    (maphash (lambda (system dependencies)
               (dolist (dep dependencies)
                 (pushnew system (gethash dep result))))
             dependency-hash)
    result))

(defparameter +direct-dependencies+ (meta-info-from-quicklisp:get-dependencies))
(defparameter +direct-dependents+ (reverse-dependencies +direct-dependencies+))
(defparameter +system->project+ (meta-info-from-quicklisp:get-system->project))

(defun direct-dependencies (system-name)
  (gethash system-name +direct-dependencies+))

(defun direct-dependents (system-name)
  (gethash system-name +direct-dependents+))

(defun reachable-nodes (root-node adjacent-nodes-fn &key test)
  (let ((adjacent-nodes (funcall adjacent-nodes-fn root-node)))
    (remove-duplicates (apply #'append
                              adjacent-nodes
                              (mapcar (lambda (adjacent-node)
                                        (reachable-nodes adjacent-node adjacent-nodes-fn :test test))
                                      adjacent-nodes))
                       :test test)))

(defun dependencies (system-name)
  (reachable-nodes system-name #'direct-dependencies :test #'string=))

(defun dependents (system-name)
  (reachable-nodes system-name #'direct-dependents :test #'string=))

(defun project-name (system-name)
  (gethash system-name +system->project+))

(defun project-names (system-names)
  (remove-duplicates (mapcar #'project-name system-names)
                     :test #'string=))

;;; --- combine dependency info with failures ---

(defun blockers (system-name failed-hash)
  "SYSTEM-NAME is a string. FAILED-HASH is a hash table
from system name to a boolean denotin whether the system load has failed."
  (remove-if-not (lambda (dependency) (gethash dependency failed-hash))
                 (dependencies system-name)))

(defun root-blockers (system-name failed-hash)
  "SYSTEM-NAME is a string. FAILED-HASH is a hash table
from system name to a boolean denotin whether the system load has failed."
  (remove-if-not (lambda (blocker-system) (null (blockers blocker-system failed-hash)))
                 (blockers system-name failed-hash)))

(defun blocked (system-name failed-hash)
  "Returns ASDF system names blocked by ASFD system
named SYSTEM-NAME. SYSTEM-NAME is a string. FAILED-HASH is a hash table
from system name to a boolean denotin whether the system load has failed."
  (when (gethash system-name failed-hash)
    (dependents system-name)))

(defun blocked-exclusively (system-name failed-hash)
  "ASDF system names for which the SYSTEM-NAME is the only blocker.
SYSTEM-NAME is a string. FAILED-HASH is a hash table
from system name to a boolean denoting whether the system load has failed."
  (when (gethash system-name failed-hash)
    (let ((list-sys-name (list system-name)))
      (remove-if-not (lambda (dep) (equal (root-blockers dep failed-hash)
                                          list-sys-name))
                     (dependents system-name)))))

;;; --- memoize the functions for better performance ---

(dolist (fn '(dependencies dependents blockers
              root-blockers blocked blocked-exclusively))
  (fare-memoization:unmemoize fn)
  (fare-memoization:memoize fn))

