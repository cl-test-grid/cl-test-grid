;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(defpackage #:sptm
  (:use :cl)
  (:export
   ;; versioned-data class
   #:versioned-data
   #:version
   #:data

   ;; generic transaction log functions
   #:roll-forward
   #:use-partially-updated ;; the restart installed by roll-forward
   #:exec-transaction
   #:record-transaction

   ;; replica
   #:replica ; the class
   #:vdata
   #:transaction-log
   #:transaction-checker
   #:local-snapshot-file
   ;; also has methods for the functions defined above: version, data
   ;; version
   ;; data
   #:sync
   #:save-local-snapshot
   #:repli-exec
   #:repli-exec-save
   #:delete-records

   ;; amazon transaction log
   #:aws-transaction-log
   #:name
   #:save-snapshot
   #:delete-records
   #:credfuscate
   #:decredfuscate))
