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
  "Accpet a hash table from system name to a list of its depencendies,
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

(dolist (fn '(dependencies dependents))
  (fare-memoization:unmemoize fn)
  (fare-memoization:memoize fn))

;;; --- combine dependency info with failures ---

(defparameter *failed-systems* nil)

(defun set-failed-systems (hash)
  "Sets global hash table from system name to a bolean denoting
wheter the system load is failed. This global table is used by the functions below"
  (setf *failed-systems* hash)
  (dolist (fn '(blockers root-blockers blocked blocked-exclusively))
    (fare-memoization:unmemoize fn)
    (fare-memoization:memoize fn)))

(defun load-failed-p (system-name)
  (assert *failed-systems* () "Please provide information about load failures via SET-FAILED-SYSTEMS.")
  (gethash system-name *failed-systems*))

(defun blockers (system-name)
  (remove-if-not #'load-failed-p (dependencies system-name)))

(defun root-blockers (system-name)
  (remove-if-not (lambda (blocker-system) (null (blockers blocker-system)))
                 (blockers system-name)))

(defun blocked (system-name)
  (when (load-failed-p system-name)
    (assert (every #'load-failed-p (dependents system-name)))
    (dependents system-name)))

(defun blocked-exclusively (system-name)
  (when (load-failed-p system-name)
    (let ((list-sys-name (list system-name)))
      (remove-if-not (lambda (dep) (equal (root-blockers dep)
                                          list-sys-name))
                      (dependents system-name)))))
