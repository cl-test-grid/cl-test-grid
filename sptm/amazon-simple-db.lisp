;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;; See LICENSE for details.

(in-package #:sptm)

(defclass simpledb-request (zaws:common-query-request zaws:query-auth-v2)
  ((duration-seconds
    :initarg :duration-seconds
    :accessor duration-seconds))
  (:default-initargs
   :host "sdb.amazonaws.com"
   :api-version "2009-04-15"))

;;; signalling errors of REST responses
;;; returned by Amazon Web Services

(defun report-aws-error (response)
  (error "Amazon Web Service returned error. HTTP status: ~A ~A.~%Body: ~A"
         (zaws:status-code response)
         (zaws:reason-phrase response)
         (zaws:content-string response)))

;;; select function, returns list of items
;;; each item has ITEM-NAME and 
;;; attributes. Attribute values
;;; may be retrieved using ITEM-ATTR.

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

(defun submit-select (query &key ((:credentials zaws:*credentials*) zaws:*credentials*) next-token)
  "Returns the response XML parsed by zaws-xml into bindings according to the select-response binder
defined above."
  (let ((response (zaws:submit (make-instance 'simpledb-request
                                              :action "Select"
                                              :action-parameters (append
                                                                  (zaws:make-parameters "ConsistentRead" "true"
                                                                                        "SelectExpression" query)
                                                                  (when next-token 
                                                                    (list (cons "NextToken" next-token))))))))
    (if (= 200 (zaws:status-code response))
        (zaws-xml:xml-bind 'select-response                        
                           (zaws:content-string response))
        (report-aws-error response))))

(defun select (query &key ((:credentials zaws:*credentials*) zaws:*credentials*))
  (zaws-xml:bvalue :items (submit-select query)))

(defun select-all (query &key ((:credentials zaws:*credentials*) zaws:*credentials*))
  (let ((next-token nil)
        (results '()))
    (loop (let ((bindings (submit-select query :next-token next-token)))
            (setf results (nconc (zaws-xml:bvalue :items bindings) results)
                  next-token (zaws-xml:bvalue :next-token bindings))
            (when (null next-token)
              (return results))))))

(defun delete-item (domain item-name &key ((:credentials zaws:*credentials*) zaws:*credentials*))
  (let* ((request (make-instance 'simpledb-request
                                 :action "DeleteAttributes"
                                 :action-parameters (zaws:make-parameters "DomainName" domain
                                                                          "ItemName" item-name)))
         (response (zaws:submit request)))
    (unless (= 200 (zaws:status-code response))
      (report-aws-error response))))

