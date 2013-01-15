;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

(defpackage #:test-grid-gae-blobstore
  (:nicknames #:tg-gae-blobstore)
  (:use :cl)
  (:export #:make-blob-store
           #:delete-blobs))

(in-package #:test-grid-gae-blobstore)

(defclass blobstore ()
  ((base-url :reader base-url
             :type string
             :initarg :base-url
             :initform (error "The :base-url parameter is required."))))

(defun make-blob-store (&rest params &key base-url)
  (declare (ignore base-url))
  (apply #'make-instance 'blobstore params))

(defconstant +max-file-length+ 100000)

(defun limit-file-length (filespec &optional (max-len +max-file-length+))
  "If the length of the file specified by FILESPEC is satisfying,
returns the FILESPEC; otherwise returns a function accepting a binary
stream as it's only argument, and writing to that stream the file content
shortened to a satisfying length: the beginning of the file,
a warning message, followed by the end of the file."
  (let ((file-len (test-grid-utils::file-byte-length filespec)))
    (if (<= file-len max-len)
        filespec
        (let* ((msg (format nil "~%~%[... SNIPPED OFF BY CL-TEST-GRID BECAUSE THIS FILE EXCEEDS THE ALLOWED MAXIMUM SIZE OF ~A BYTES: ~A ...]~%~%"
                            max-len
                            file-len)))
          (assert (< (length msg) max-len)
                  (max-len)
                  "max-len is less than the message we are going to substitute instead of the overlonged snippet.")
          (warn "The file ~A exceeds the allowed maximum length of ~A bytes: ~A. Submitting a shortened version of the file to the online storage." filespec max-len file-len)
          #'(lambda (stream)
              (let* ((beginning-len (floor (/ (- max-len (length msg))
                                              2)))
                     (end-len (- max-len beginning-len (length msg)))
                     (buf (make-array (max beginning-len end-len)
                                      :element-type '(unsigned-byte 8))))
                (with-open-file (f filespec :element-type '(unsigned-byte 8))
                  ;; copy the biginning of the file to buf
                  (read-sequence buf f :start 0 :end beginning-len)
                  (write-sequence buf stream :start 0 :end beginning-len)
                  ;; copy the message
                  (loop for ch across msg
                     for buf-pos from beginning-len by 1
                     do (write-byte (coerce (rem (char-code ch) 255)
                                            '(unsigned-byte 8))
                                    stream))
                  ;; copy the ending of the file
                  (file-position f (- file-len end-len))
                  (read-sequence buf f :start 0 :end end-len)
                  (write-sequence buf stream :start 0 :end end-len))))))))

;; test for limit-file-length:
;;
;; (let ((fun (limit-file-length #P"C:\\Users\\anton\\projects\\cl-test-grid\\test.txt" 500)))
;;   (with-open-file (out #P"C:\\Users\\anton\\projects\\cl-test-grid\\test2.txt"
;;                      :direction :output
;;                      :if-does-not-exist :create
;;                      :if-exists :overwrite
;;                      :element-type '(unsigned-byte 8))
;;     (if (functionp fun)
;;         (funcall fun out)
;;         fun)))


(defmethod test-grid-blobstore:submit-files ((blobstore blobstore) id-pathname-alist)
  (flet ((submit-impl (id-pathname-alist-part)
           (let* (;; Google App Engine does not allow to submit blobs to a constant URL,
                  ;; we need to perform a separate request to our servlet, which will
                  ;; generate an URL where we can upload files.
                  (upload-url (drakma:http-request (format nil "~A/upload-url" (base-url blobstore))
                                                   :content-type "text/plain"))
                  ;; Now prepare POST parameters for the main submit request,
                  ;; according the drakma API for file posting.
                  ;;
                  ;; Namely, ensure the IDs are strings and add "text/plain" content type.
                  ;;
                  ;; Example: if ID-PATHNAME-ALIST is
                  ;;   ((:alexandria #P"/logs/alexandria.log") ... )
                  ;; convert it to
                  ;;   (("alexandria" #P"/logs/alexandria.log" :content-type "text/plain") ... )
                  (post-params (mapcar #'(lambda (elem)
                                           (cons (string-downcase (car elem))
                                                 (list (limit-file-length (cdr elem))
                                                       :filename (file-namestring (cdr elem))
                                                       :content-type "text/plain")))
                                       id-pathname-alist-part)))
             (multiple-value-bind (stream status-code headers uri stream2 must-close reason-phrase)
                 (drakma:http-request upload-url
                                      :method :post
                                      :content-length t
                                      :parameters post-params
                                      :want-stream t)
               (declare (ignore headers uri stream2 must-close))
               (cond ((/= 200 status-code)
                      (error "Error uploading files, the HTTP response code ~A: ~A" status-code reason-phrase))
                     (t (with-open-stream (stream stream)
                          (log:info "next ~A files are uploaded" (length id-pathname-alist-part))
                          ;; And read the response
                          (test-grid-utils::safe-read stream))))))))
    (let* ( ;;Split the files submitted into batches by < 500 elements
           ;; to workaround GAE blobstore issue: https://code.google.com/p/googleappengine/issues/detail?id=8032
           (batches (test-grid-utils::split-list id-pathname-alist 100))
           (response (mapcan #'submit-impl batches)))
      ;; Now RESPONSE contains an alist of
      ;; (<stringified ID> . <blob key>)  pairs.
      ;; For example:
      ;;    (("alexandria" . "cJVA1Klp7o-Lz2Cc6KuPcg") ...)
      ;; As in the original id-pathname-alist the IDs might be represented
      ;; as symbols, lets return response with IDs in the original form, e.g.
      ;;    ((:alexandria . "cJVA1Klp7o-Lz2Cc6KuPcg") ...)
      ;;
      ;; During the conversion we also check that we got blobkeys for
      ;; all the files we submitted.
      (flet ((get-blobkey (for-id)
               (or (cdr (assoc for-id response :test #'string-equal))
                   (error "The response does not contain a blobkey for the ~A" for-id))))
        (mapcar (lambda (id-pathname-pair)
                  (cons (car id-pathname-pair)
                        (get-blobkey (car id-pathname-pair))))
                id-pathname-alist)))))

(defmethod test-grid-blobstore:submit-run-info ((blobstore blobstore) run-info)
  (assert (not (null run-info)))
  (let ((response (drakma:http-request (format nil "~A/submit-run-info" (base-url blobstore))
                                       :method :post
                                       :parameters `(("run-info" . ,(prin1-to-string run-info))))))
    (when (not (eq :ok (with-input-from-string (s response)
                         (test-grid-utils::safe-read s))))
      (error "Error submitting run info to the server. Unexpected response: ~A." response))))

(defmethod test-grid-blobstore:tell-admin ((blobstore blobstore) subject body)
  (assert (not (null subject)))
  (setf body (or body ""))
  (let ((response (drakma:http-request (format nil "~A/tell-admin" (base-url blobstore))
                                       :method :post
                                       :parameters `(("subject" . ,subject)
                                                     ("body" . ,body)))))
    (when (not (eq :ok (with-input-from-string (s response)
                         (test-grid-utils::safe-read s))))
      (error "Error sending message to admin. Unexpected response: ~A." response))))

(defparameter *logs-to-keep*
  (list))

(defun delete-blobs (blobstore blob-keys)
  (flet ((delete-batch (blob-keys)
           (multiple-value-bind (body status-code headers uri stream2 must-close reason-phrase)
               (drakma:http-request (format nil "~A/delete-blobs" (base-url blobstore))
                                    :method :post
                                    :parameters `(("keys" . ,(format nil "~{~A~^,~}" blob-keys))))
             (declare (ignore headers uri stream2 must-close))
             (if (/= 200 status-code)
                 (error "Error deleting blobs, the HTTP response code ~A: ~A" status-code reason-phrase)
                 body))))
    (let* ((filtered-keys (set-difference blob-keys *logs-to-keep* :test #'string=))
           (total (length filtered-keys))
           (done 0))
      (dolist (batch (test-grid-utils::split-list filtered-keys 50))
        (delete-batch batch)
        (incf done (length batch))
        (log:info "~A/~A blobs deleted" done total)))))