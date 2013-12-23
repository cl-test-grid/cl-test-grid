;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(in-package #:test-grid-reporting)

;;; Note is either a text or a ticket reference
(defclass ticket () ())
(defclass launchpad-ticket (ticket)
  ((id :type string
       :accessor id
       :initarg :id
       :initform (error ":id is required"))))

(defun note-body-p (obj)
  (or (stringp obj)
      (typep obj 'ticket)))

(defmethod print-object ((ticket launchpad-ticket) stream)
  (print-unreadable-object (ticket stream :type t :identity t)
    (format stream "~S" (id ticket))))

(defgeneric ticket-url (ticket))
(defmethod ticket-url ((ticket launchpad-ticket))
  (concatenate 'string
               "http://bugs.launchpad.net/bugs/"
               (id ticket)))

(defun lp-ticket (id)
  (make-instance 'launchpad-ticket :id id))

;;; Note database

(defun make-note-db ()
  (make-hash-table :test 'equal))

(defun set-note (db fields field-values body)
  (let ((index (or (gethash fields db)
                    (setf (gethash fields db)
                          (make-hash-table :test 'equal)))))
    (setf (gethash field-values index) body)))

(defun note (db fields field-values)
  (let ((index (gethash fields db)))
    (when index
      (gethash field-values index))))

(let ((db (make-note-db)))
  (set-note db '(lisp) '("sbcl") "this is a note for SBCL")
  (assert (string= (note db '(lisp) '("sbcl"))
                   "this is a note for SBCL")))

(defun notes (db result)
  (let ((notes nil))
    (maphash (lambda (fields index)
               (let* ((field-vals (mapcar (lambda (field)
                                           (funcall field result))
                                          fields))
                      (note (gethash field-vals index)))
                 (when note
                   (push note notes))))
             db)
    notes))

(defun fill-notes (note-spec-list)
  (flet ((as-list (val)
           (if (consp val)
               val
               (list val))))
    (let ((db (make-note-db)))
      (labels ((fill-rec (fields field-vals spec)
                 (if (note-body-p spec)
                     (set-note db fields field-vals spec)
                     (let* ((cur-field (first spec))
                            (cur-field-vals (as-list (second spec)))
                            (fields (cons cur-field fields)))
                       (dolist (cur-field-val cur-field-vals)
                         (let ((field-vals (cons cur-field-val field-vals)))
                           (dolist (subspec (cddr spec))
                             (fill-rec fields field-vals subspec))))))))
        (dolist (spec note-spec-list)
          (fill-rec '() '() spec)))
      db)))

(defparameter *note-db*
  (fill-notes `((lib-world "quicklisp 2013-08-13"
                 (libname (:series)
                  (lisp-impl-type :acl
                    ,(lp-ticket "1249658")))
                 (libname :com.informatimago
                  (failure-p t "author informed"))
                 (libname :asdf-dependency-grovel
                  (system-name "asdf-dependency-grovel"
                   (failure-p t "needs ASDF 3")))
                 (libname :xcvb
                  (failure-p t
                   (lisp ("acl-9.0a-win-x86" "ccl-1.8-f95-win-x64" "ccl-1.8-f95-win-x86" "sbcl-1.0.57.0.debian-linux-x64")
                     "needs ASDF 3")))
                 (libname :exscribe
                  (failure-p t
                    "needs ASDF 3"))
                 (libname (:periods :cambl)
                  (failure-p t
                    ,(lp-ticket "1229050"))))
                (lib-world "quicklisp 2013-10-03"
                 (libname (:series)
                  (lisp-impl-type :acl
                    ,(lp-ticket "1249658")))
                 (libname (:periods :cambl)
                  (lisp-impl-type :acl
                    "new dependency SERIES doesn't work on ACL"))
                 (libname :cl-annot
                   (lisp-impl-type :cmu
                     ,(lp-ticket "1242490")))
                 (libname :cl-autowrap
                   (lisp-impl-type :ccl
                     ,(lp-ticket "1242492")))
                 (libname (:cl-gdata :cl-generic-arithmetic :sexml :string-case
                           :memoize :macroexpand-dammit :conduit-packages
                           :docbrowser)
                  (failure-p t
                    ,(lp-ticket "1242500")))
                 (libname :cl-grace
                  (lisp-impl-type (:acl :abcl)
                    "Not a problem. ACL and ABCL are not supported anyway."))
                 (libname :cl-parser-combinators
                  (lisp-impl-type :ecl
                   (failure-p t
                    ,(lp-ticket "1243531"))))
                 (libname (:cl-redis :cl-secure-read :rutils)
                  (lisp ("ecl-13.4.1-0e93edfc-win-x86-bytecode"
                         "ecl-13.4.1-0e93edfc-win-x86-lisp-to-c"
                         "abcl-1.2.1-fasl42-macosx-x64")
                   (failure-p t
                    ,(lp-ticket "1243540"))))
                 (libname :opticl
                  (lisp ("cmu-snapshot-2013-04__20d_unicode_-linux-x86")
                    (failure-p t
                      ,(lp-ticket "1244452"))))
                 (system-name "lil"
                   (failure-p t "new LIL version requires ASDF 3")))
                (lib-world "quicklisp 2013-11-11"
                 (system-name "asdf-package-system"
                   (failure-p t "needs ASDF 3"))
                 (libname (:caveman :cl-emb :cl-project)
                  (lisp ("acl-9.08-linux-x64"
                         "acl-9.08-linux-x86"
                         "acl-9.08s-linux-x64"
                         "acl-9.08s-linux-x86"
                         "acl-9.0m8-linux-x64"
                         "acl-9.0m8-linux-x86"
                         "acl-9.0m8s-linux-x64"
                         "acl-9.0m8s-linux-x86"
                         "ccl-1.8-f95-win-x64"
                         "ccl-1.8-f95-win-x86"
                         "clisp-2.49-win-x86"
                         "sbcl-1.1.0.36.mswinmt.1201-284e340-win-x64"
                         "sbcl-1.1.0.36.mswinmt.1201-284e340-win-x86")
                   ,(lp-ticket "1258873")))
                 (libname :cl-annot
                   (result-spec ((:whole-test-suite :fail))
                     ,(lp-ticket "1258876")))
                 (lisp ("acl-9.0-linux-x64"
                        "acl-9.0-linux-x86"
                        "acl-9.08-linux-x64"
                        "acl-9.08-linux-x86"
                        "acl-9.08s-linux-x64"
                        "acl-9.08s-linux-x86"
                        "acl-9.0s-linux-x64"
                        "acl-9.0s-linux-x86"
                        "ccl-1.8-f95-win-x64"
                        "ccl-1.8-f95-win-x86"
                        "ccl-1.9-f96-linux-x64"
                        "ccl-1.9-f96-linux-x86"
                        "ccl-1.9-f96-macosx-x64"
                        "ccl-1.9-f96-macosx-x86")
                  (failure-p t
                    (libname (:clsql-helper :cl-csv)
                     ,(lp-ticket "1258883"))
                    (system-name ("data-table-clsql" "function-cache-clsql")
                     ,(lp-ticket "1258883"))))
                 (libname (:hunchentoot :hunchentoot-auth :hunchentoot-cgi :hunchentoot-vhost
                           :cl-dropbox :cl-oauth :cl-paypal :amazon-ecs :ayah-captcha
                           :cl-cheshire-cat :cl-server-manager :cl-webdav :cxml-rpc :ext-blog
                           :firephp :formlets :gtfl :hh-web :ht-simple-ajax :smackjack
                           :restas :restas-directory-publisher :restas.file-publisher :rpc4cl)
                  (failure-p t
                   (lisp-impl-type :clisp
                    ,(lp-ticket "1258948"))))
                 (system-name ("cl-twit-repl" "cl-twitter"
                               "clack-handler-hunchentoot" "clack-middleware-oauth")
                  (failure-p t
                   (lisp-impl-type :clisp
                    ,(lp-ticket "1258948"))))
                 (libname (:cl-html-parse :cl-openid :cl-web-crawler :nekthuth)
                  (failure-p t
                   (lisp-impl-type (:abcl :acl :cmu :ecl)
                    ,(lp-ticket "1252283"))))
                 (system-name "dbd-sqlite3"
                  (failure-p t
                    (lisp ("ecl-12.12.1-unknown-linux-i686-bytecode"
                           "ecl-13.4.1-94e04b54-linux-x64-bytecode"
                           "ecl-13.5.1-237af2e8-linux-i686-bytecode"
                           "ecl-13.5.1-unknown-linux-i686-bytecod")
                     ,(lp-ticket "1258995"))))
                 (libname :cl-launch
                  (lisp ("abcl-1.1.1-fasl39-linux-x86"
                         "clisp-2.49-unix-i386"
                         "clisp-2.49-unix-x64"
                         "clisp-2.49-win-x86"
                         "ecl-12.12.1-unknown-linux-i686-lisp-to-c"
                         "sbcl-1.0.57.0.debian-linux-x64"
                         "sbcl-1.1.0.36.mswinmt.1201-284e340-win-x64"
                         "sbcl-1.1.0.36.mswinmt.1201-284e340-win-x86")
                   (failure-p t
                     "Needs newer ASDF")))
                 (system-name ("cl-mongo" "twitter-mongodb-driver")
                  (failure-p t
                   (lisp-impl-type :ecl
                     ,(lp-ticket "1259029"))))
                 (libname (:cl-redis :cl-secure-read)
                  (failure-p t
                   ,(lp-ticket "1243540")))
                 (libname :cl-tuples (failure-p t ,(lp-ticket "1259051")))
                 (libname (:coleslaw :inferior-shell)
                  (lisp ("abcl-1.2.0-fasl42-linux-x86"
                         "abcl-1.2.1-fasl42-linux-x64"
                         "abcl-1.2.1-fasl42-macosx-x64"
                         "ccl-1.9-f96-linux-x64"
                         "ccl-1.9-f96-linux-x86"
                         "ccl-1.9-f96-macosx-x64"
                         "ccl-1.9-f96-macosx-x86"
                         "clisp-2.49-win-x86"
                         "ecl-12.12.1-unknown-linux-i686-bytecode"
                         "ecl-13.4.1-0e93edfc-win-x86-bytecode"
                         "ecl-13.4.1-0e93edfc-win-x86-lisp-to-c"
                         "ecl-13.4.1-94e04b54-linux-x64-bytecode"
                         "ecl-13.4.1-94e04b54-linux-x64-lisp-to-c"
                         "ecl-13.5.1-unknown-linux-i686-bytecode"
                         "ecl-13.5.1-unknown-linux-i686-lisp-to-c"
                         "sbcl-1.1.11-linux-x86")
                   (failure-p t
                          "inferior-shell needs newer ASDF")))
                 (libname :gendl
                  (system-name "surf"
                   (lisp "ccl-1.9-f96-linux-x86"
                     ,(lp-ticket "1261297"))))
                 (libname (:eager-future2 :intercom :stmx :thread.comm.rendezvous :myweb)
                  (lisp "ecl-13.4.1-94e04b54-linux-x64-lisp-to-c"
                    (failure-p t "Not a regresssion; bug #261 in old ECL shows up due to differrent compilation order")))
                 (system-name "funds"
                   (lisp ("sbcl-1.0.57.0.debian-linux-x64"
                          "clisp-2.49-unix-x64")
                    (failure-p t
                      "Not a regression, result of a different compilation order")))
                 (system-name "kl-verify"
                  (lisp ("cmu-snapshot-2013-04__20d_unicode_-linux-x86")
                    (failure-p t "Not a regression, result of a different compilation order")))
                 (libname (:hu.dwim.perec :jenkins :metacopy :metatilities :moptilities
                           :tinaa :weblocks :weblocks-stores :weblocks-tree-widget
                           :rpm)
                  (fail-condition-type ("ASDF:MISSING-DEPENDENCY-OF-VERSION"
                                        "ASDF/FIND-COMPONENT:MISSING-DEPENDENCY-OF-VERSION")
                   ,(lp-ticket "1262020")))
                 (libname :l-math
                   (lisp-impl-type (:acl :clisp :ecl)
                    (failure-p t
                      ,(lp-ticket "1262026"))))
                 (libname :lisp-interface-library
                  (lisp ("ccl-1.8-f95-win-x64"
                         "ccl-1.8-f95-win-x86"
                         "ccl-1.9-f96-linux-x64"
                         "ccl-1.9-f96-linux-x86"
                         "ccl-1.9-f96-macosx-x64"
                         "ccl-1.9-f96-macosx-x86"
                         "ecl-12.12.1-unknown-linux-i686-bytecode"
                         "ecl-13.4.1-0e93edfc-win-x86-bytecode"
                         "ecl-13.4.1-0e93edfc-win-x86-lisp-to-c"
                         "ecl-13.4.1-94e04b54-linux-x64-bytecode"
                         "ecl-13.4.1-94e04b54-linux-x64-lisp-to-c"
                         "ecl-13.5.1-237af2e8-linux-i686-bytecode"
                         "ecl-13.5.1-237af2e8-linux-i686-lisp-to-c"
                         "ecl-13.5.1-unknown-linux-i686-bytecode"
                         "ecl-13.5.1-unknown-linux-i686-lisp-to-c"
                         "sbcl-1.0.57.0.debian-linux-x64"
                         "sbcl-1.1.0.36.mswinmt.1201-284e340-win-x64"
                         "sbcl-1.1.0.36.mswinmt.1201-284e340-win-x86")
                   (failure-p t
                          "requires ASDF 3 or later")))
                 (libname :rutils
                  (failure-p t
                   ,(lp-ticket "1243540")))
                 (libname (:smtp4cl :plain-odbc :net4cl)
                  (lisp-impl-type :sbcl
                   (failure-p t
                     "Not a regression; constant redifinition shows up due to different compilation order.")))))))

#|
 (notes *note-db* (first (subset *all-results*
                                 (lambda (r) (and
                                              (eq (libname r) :cambl)
                                              (failure-p r)
                                              (string= (lib-world r) "quicklisp 2013-08-13"))))))

 (time (dolist (r *all-results*)
         (notes *note-db* r)))
|#
