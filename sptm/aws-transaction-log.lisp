;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(in-package #:sptm)

(defclass aws-transaction-log ()
  ((name :type string
         :accessor name
         :initarg :name
         :initform (error ":name is required"))
   (credentials :type cons
                :accessor credentials
                :initarg :credentials
                :initform (error ":credentials are required"))
   (s3-bucket :type string
              :accessor s3-bucket
              :initarg :s3-bucket
              :initform (error ":s3-bucket is required"))
   (simpledb-domain :type string
                    :accessor simpledb-domain
                    :initarg :simpledb-domain
                    :initform (error ":simpledb-domain is required"))))

(zaws-xml:defbinder error-response
  ("Response"
   ("Errors"
    (sequence :errors
              ("Error" ("Code" (zaws-xml:bind :code))
                       ("Message" (zaws-xml:bind :message))
                       zaws-xml:skip-rest)))
   zaws-xml:skip-rest))

(defun conditional-check-failed-p (put-attribute-response)
  (and (= 409 (zaws:status-code put-attribute-response))
       (member "ConditionalCheckFailed"
               (zaws-xml:bvalue :errors
                                (zaws-xml:xml-bind 'error-response
                                                   (zaws:content-string put-attribute-response)))
               :key (alexandria:curry #'zaws-xml:bvalue :code)
               :test #'string=)))

(defun version-str (version-number)
  (format nil "~9,'0D" version-number))

(defun max-version-str ()
  "999999999")

(defun parse-version-number (simpledb-item-name)
  (let* ((name-start (1+ (search "-" simpledb-item-name)))
         (name-end (search "-" simpledb-item-name :start2 name-start)))
    (parse-integer simpledb-item-name
                   :start name-start
                   :end name-end)))

(assert (= 132 (parse-version-number "test-000000132-tx")))
(assert (= 132 (parse-version-number "test-000000132-snapshot")))

(defgeneric serialize-to-string (log object)
  (:method ((log aws-transaction-log) object)
    (with-standard-io-syntax 
      (with-output-to-string (s)
        (prin1 object s)))))

(defgeneric deserialize-from-string (log string)
  (:method ((log aws-transaction-log) string)
    (with-input-from-string (s string)
      (test-grid-utils::safe-read s))))

(defun gzip-string (str)
  "Returns byte vector"
  (gzip-stream:gzip-sequence (babel:string-to-octets str)))

(defun gunzip-string (byte-vector)
  (babel:octets-to-string (gzip-stream:gunzip-sequence byte-vector)))

(assert (string= "abc" (gunzip-string (gzip-string "abc"))))

(defun short-timestamp (&optional (time (get-universal-time)))
  "Returns a short timestamp string in GMT time."
  (multiple-value-bind (sec min hour day month year day-of-week)
      (decode-universal-time time 0)
    (declare (ignore day-of-week))
    (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
            year month day hour min sec)))

(defvar *suffix-random-state* nil)

(defun random-suffix ()
  (when (null *suffix-random-state*)
    (setf *suffix-random-state* (make-random-state t)))
  (format nil "~36,3,'0r" (random #.(expt 36 3) *suffix-random-state*)))

(defgeneric unique-s3-object-name (log)
  (:method ((log aws-transaction-log))
    (format nil "~A-~A.~A" (name log) (short-timestamp) (random-suffix))))

(defmethod persist-funcall ((log aws-transaction-log) func-symbol args-without-data-arg)
  (let ((value (gzip-string (serialize-to-string log (list func-symbol args-without-data-arg))))
        (name (unique-s3-object-name log)))
    (zs3:put-vector value (s3-bucket log) name
                    :access-policy :private
                    :content-type "text/plain"
                    :content-encoding "gzip"
                    :credentials (credentials log))
    name))

(defmethod commit-version ((log aws-transaction-log) version-number s3-object-name)
  (let* ((zaws:*credentials* (credentials log))
         (request (make-instance 'simpledb-request
                                 :action "PutAttributes"
                                 :action-parameters (zaws:make-parameters "DomainName" (simpledb-domain log)
                                                                          "ItemName" (format nil "~A-~A-tx"
                                                                                             (name log)
                                                                                             (version-str version-number))
                                                                          "Attribute.1.Name" "s3objectname"
                                                                          "Attribute.1.Value" s3-object-name
                                                                          "Expected.1.Exists" "false"
                                                                          "Expected.1.Name" "s3objectname")))
         (response (zaws:submit request)))
    (cond
      ((conditional-check-failed-p response) nil)
      ((= 200 (zaws:status-code response)))
      (t (report-aws-error response)))))

(defclass aws-transaction ()
  ((transaction-log :type aws-transaction-log
                    :accessor transaction-log
                    :initarg :transaction-log
                    :initform (error ":transaction-log is required"))
   (version :type fixnum
            :accessor version
            :initarg :version
            :initform (error ":version is required"))
   (s3-object-name :type string
                   :accessor s3-object-name
                   :initarg :s3-object-name
                   :initform (error ":s3-object-name is required"))
   (s3-object-content :type t
                      :initform nil)))

(defmethod print-object ((tx aws-transaction) stream)
  (print-unreadable-object (tx stream :type t :identity t)
    (format stream "~A ~S ..." (version tx) (func tx))))

(defmethod func ((transaction aws-transaction))
  (first (s3-object-content transaction)))

(defmethod args ((transaction aws-transaction) data)
  (cons data (second (s3-object-content transaction))))

(defun s3-object-content (aws-transaction)
  (when (null (slot-value aws-transaction 's3-object-content))
    (setf (slot-value aws-transaction 's3-object-content)
          (let ((log (transaction-log aws-transaction)))
            (deserialize-from-string log
                                     (gunzip-string (zs3:get-vector (s3-bucket log)
                                                                    (s3-object-name aws-transaction)
                                                                    :credentials (credentials log)))))))
  (slot-value aws-transaction 's3-object-content))

(defmethod list-transactions ((log aws-transaction-log) after-version)
  (flet ((make-transaction (item)
           (make-instance 'aws-transaction
                          :transaction-log log
                          :version (parse-version-number (item-name item))
                          :s3-object-name (item-attr item "s3objectname"))))
    (mapcar #'make-transaction
            (select-all (format nil
                                "select * from cltestgrid where itemName() like '%-tx' and itemName() > '~A-~A-tx' and itemName() < '~A-~A-tx' order by itemName() limit 2500"
                                (name log)
                                (version-str after-version)
                                (name log)
                                (max-version-str))          
                        :credentials (credentials log)))))

(defun border-transaction-item (log max-or-min)
  (first (select (format nil
                         "select * from cltestgrid where itemName() like '~A-%' and itemName() like '%-tx' order by itemName() ~A limit 1"
                         (name log)
                         (ecase max-or-min
                           (:max "desc")
                           (:min "asc")))
                 :credentials (credentials log))))

(defun min-transaction-item (log)
  (border-transaction-item log :min))

(defun max-transaction-item (log)
  (border-transaction-item log :max))

(defmethod min-transaction-version ((log aws-transaction-log))
  (let ((item (or (min-transaction-item log)
                  (error "min-transaction-version is invoked on emtpy log"))))
    (parse-version-number (item-name item))))

(defmethod max-transaction-version ((log aws-transaction-log))
  (let ((item (or (max-transaction-item log)
                  (error "max-transaction-version is invoked on emtpy log"))))
    (parse-version-number (item-name item))))

(defmethod empty-p ((log aws-transaction-log))
  (null (min-transaction-item log)))


(defun last-snapshot-item (log)
  (first (select (format nil
                         "select * from cltestgrid where itemName() like '~A-%' and itemName() like '%-snapshot' order by itemName() desc limit 1"
                         (name log))
                 :credentials (credentials log))))

(defmethod snapshot-version ((log aws-transaction-log))
  (let ((item (last-snapshot-item log)))
    (if item
        (parse-version-number (item-name item))
        0)))

(defmethod get-snapshot ((log aws-transaction-log))
  (let ((item (last-snapshot-item log)))
    (if item
        (make-instance 'versioned-data
                       :version (parse-version-number (item-name item))
                       :data (deserialize-from-string log
                                                      (gunzip-string (zs3:get-vector (s3-bucket log)
                                                                                     (item-attr item "s3objectname")
                                                                                     :credentials (credentials log)))))
        (make-instance 'versioned-data))))

(defmethod save-snapshot ((log aws-transaction-log) versioned-data)
  (let* (;; simple DB item name according the same pattern as transaction item
         ;; names: <log-name>-<version>-snapshot
         (snapshot-db-name (format nil
                                   "~A-~A-snapshot"
                                   (name log)
                                   (version-str (version versioned-data))))
         ;; S3 object name as <log-name>-snaphot-<version>, so that all
         ;; snapshots are grouped together and easy to find in AWS console
         (snapshot-s3-name (format nil "~A-snapshot-~A"
                                   (name log)
                                   (version-str (version versioned-data)))))
    
    (zs3:put-vector (gzip-string (serialize-to-string log (data versioned-data)))
                    (s3-bucket log)
                    snapshot-s3-name
                    :access-policy :private
                    :content-type "text/plain"
                    :content-encoding "gzip"
                    :credentials (credentials log))

    (let* ((zaws:*credentials* (credentials log))
           (request (make-instance 'simpledb-request
                                   :action "PutAttributes"
                                   :action-parameters (zaws:make-parameters "DomainName" (simpledb-domain log)
                                                                            "ItemName" snapshot-db-name
                                                                            "Attribute.1.Name" "s3objectname"
                                                                            "Attribute.1.Value" snapshot-s3-name
                                                                            "Attribute.1.Replace" "true")))
           (response (zaws:submit request)))
      (unless (= 200 (zaws:status-code response))
        (report-aws-error response)))))

(defun delete-records (log &key (from-version 0) (below-version nil))
  (dolist (item (select-all (format nil
                                    "select * from cltestgrid where itemName() >= '~A-~A' and itemName() < '~A-~A' order by itemName() limit 2500"
                                    (name log)
                                    (version-str from-version)
                                    (name log)
                                    (if below-version (version-str below-version) (max-version-str)))
                            :credentials (credentials log)))
    (zs3:delete-object (s3-bucket log)
                       (item-attr item "s3objectname")
                       :credentials (credentials log))
    (delete-item (simpledb-domain log)
                 (item-name item)
                 :credentials (credentials log))))
