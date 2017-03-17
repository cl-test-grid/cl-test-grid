;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(in-package #:sptm)

(defclass simpledb-request (zaws:common-query-request zaws:query-auth-v2)
  ((duration-seconds
    :initarg :duration-seconds
    :accessor duration-seconds))
  (:default-initargs
   :api-version "2009-04-15"))

;;; signalling errors of REST responses
;;; returned by Amazon Web Services

(defun report-aws-error (response)
  (error "Amazon Web Service returned error. HTTP status: ~A ~A.~%Body: ~A"
         (zaws:status-code response)
         (zaws:reason-phrase response)
         (zaws:content-string response)))

;;; All the functions below accept parameter OPTIONS,
;;; which is a plist with :credentials property holding
;;; the Amazon Web Service credentials, and
;;; any other keywords arguments which may be passed
;;; to MAKE-INSTANCE for SIMPLEDB-REQUEST.
;;;
;;; Here is how OPTIONS are handled:
(defmacro with-sdb-credentials ((options) &body body)
  `(let ((zaws:*credentials* (getf ,options :credentials)))
     ,@body))


(defun submit-sdb-request (options action param-key-vals)
  (let ((request (apply #'make-instance
                        'simpledb-request
                        :action action
                        :action-parameters (apply #'zaws:make-parameters param-key-vals)
                        :allow-other-keys t
                        options))
        (retries 3)
        (sleep-times '(20 60 200))
        (cur-retry 0))
    (with-sdb-credentials (options)
      (tagbody
       retry
         (handler-bind
             ((serious-condition
               (lambda  (c)
                 (multiple-value-bind (sec min hour)
                     (decode-universal-time (get-universal-time))
                   (format *error-output*
                           "[~2,'0d:~2,'0d:~2,'0d] Error during submit-sdb-request: ~A~%"
                           hour min sec c))
                 (when (<= (incf cur-retry) retries)
                   (let ((sleep-time (nth (1- cur-retry) sleep-times)))
                     (format *error-output*
                             "Sleeping ~A seconds before retry ~A/~A~%"
                             sleep-time cur-retry retries)
                     (sleep sleep-time))
                   (go retry)))))
           (return-from submit-sdb-request
             (zaws:submit request)))))))

;;; SELECT function, returns a list of items.
;;; Each item has ITEM-NAME and 
;;; attributes. Attribute values
;;; may be retrieved using ITEM-ATTR.
;;;
;;; Note, we do not support the ability of SimpleDB
;;; to store several values of the same attributes.
;;; We always store only single value for an attribute
;;; and our SELECT function relies on this.

(zaws-xml:defbinder select-response
  ("SelectResponse"
   ("SelectResult"
    (sequence :items
              ("Item" ("Name" (zaws-xml:bind :name))
                      (zaws-xml:optional (sequence :attributes
                                                   ("Attribute" ("Name" (zaws-xml:bind :name))
                                                                ("Value" (zaws-xml:bind :value)))))))
    (zaws-xml:optional ("NextToken" (zaws-xml:bind :next-token))))
   zaws-xml:skip-rest))

(defun item-name (item)
  (zaws-xml:bvalue :name item))

(defun item-attr (item attribute-name)
  (zaws-xml:bvalue :value 
                   (or (find attribute-name
                             (zaws-xml:bvalue :attributes item)
                             :key (alexandria:curry #'zaws-xml:bvalue :name)
                             :test #'string=)
                       (error "Attribute ~S not found: ~S" attribute-name item))))

(defun submit-select (query options &key next-token)
  "Returns the response XML parsed by zaws-xml into bindings according to the select-response binder
defined above."
  (let ((response (submit-sdb-request options
                                      "Select"
                                      (append (list "ConsistentRead" "true"
                                                    "SelectExpression" query)
                                              (when next-token
                                                (list "NextToken" next-token))))))
    (if (= 200 (zaws:status-code response))
        (zaws-xml:xml-bind 'select-response                        
                           (zaws:content-string response))
        (report-aws-error response))))

(defun select (query options)
  (zaws-xml:bvalue :items (submit-select query options)))

(defun select-all (query options)
  "The function allows to overcome the Amazon SimpleDB limitation
that SELECT response returns at maximum 2500 items.

Repeats the specified query while the repsonse contains NextToken
element (the NextToken received is posted with every subsequent response).

It is important to know how the 'limit' argument of SELECT query
and the NextToken response element interoperate.

Suppose we have 10 items in the domain 'mydomain' and 
execute query 'select * from mydomain limit 2'.

The SELECT-ALL for this query will return 10 items,
and will need to submit this query 5 times to Amazon.

This is because NextToken is always added to response
if there are more items matching the select, even if
the response alreay delivered limit of items.
I.e. limit serves as a maximum batch size."
  (let ((next-token nil)
        (results '()))
    (loop (let ((bindings (submit-select query options :next-token next-token)))
            (setf results (nconc results (zaws-xml:bvalue :items bindings))
                  next-token (zaws-xml:bvalue :next-token bindings))
            (when (null next-token)
              (return results))))))

(defun select-first (query options)
  "The QUERY is expected to be 'SELECT ... LIMIT 1'.

The problem this function solves, is that Amazon Simple DB
seems sometimes return empty result, even if matching records
exist (maybe due to query timeout, or other problems).

The solution is to repeat request if result is empty, but NextToken is present." 
  (let ((next-token nil))
    (loop (let* ((bindings (submit-select query options :next-token next-token))
                 (results (zaws-xml:bvalue :items bindings)))
            (setf next-token (zaws-xml:bvalue :next-token bindings))
            (if results
                (return (first results))
                (unless next-token
                  (return nil)))))))

(defun delete-item (domain item-name options)
  (let ((response (submit-sdb-request options "DeleteAttributes" (list "DomainName" domain
                                                                       "ItemName" item-name))))
    (unless (= 200 (zaws:status-code response))
      (report-aws-error response))))

(defun create-simpledb-domain (domain-name options)
  (let* ((response (submit-sdb-request options "CreateDomain" (list "DomainName" domain-name))))
    (unless (= 200 (zaws:status-code response))
      (report-aws-error response))))
