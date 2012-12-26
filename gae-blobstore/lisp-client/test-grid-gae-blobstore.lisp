;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

(defpackage #:test-grid-gae-blobstore
  (:use :cl)
  (:export #:make-blob-store))

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

(defun send-shortened-file (filespec out-stream file-len len-limit)
  (let* ((msg (format nil "~%~%[... SNIPPED OFF BY CL-TEST-GRID BECAUSE THIS FILE EXCEEDS THE ALLOWED MAXIMUM SIZE OF ~A BYTES: ~A ...]~%~%"
                      len-limit
                      file-len)))
    (assert (< (length msg) len-limit)
            (len-limit)
            "max-len is less than the message we are going to substitute instead of the overlonged snippet.")
    (warn "The file ~A exceeds the allowed maximum length of ~A bytes: ~A. Submitting a shortened version of the file to the online storage." filespec len-limit file-len)
    (let* ((beginning-len (floor (/ (- len-limit (length msg))
                                    2)))
           (end-len (- len-limit beginning-len (length msg)))
           (buf (make-array (max beginning-len end-len)
                            :element-type '(unsigned-byte 8))))
      (with-open-file (f filespec :element-type '(unsigned-byte 8))
        ;; copy the biginning of the file to buf
        (read-sequence buf f :start 0 :end beginning-len)
        (write-sequence buf out-stream :start 0 :end beginning-len)
        ;; copy the message
        (write-sequence (flexi-streams:string-to-octets msg :external-format :utf-8)
                        out-stream)
        ;; copy the ending of the file
        (file-position f (- file-len end-len))
        (read-sequence buf f :start 0 :end end-len)
        (write-sequence buf out-stream :start 0 :end end-len)))))

(defun send-file (filespec out-stream)
  (with-open-file (in-stream filespec :element-type '(unsigned-byte 8))
    (let ((buf (make-array 8192 :element-type '(unsigned-byte 8))))
      (loop
         (let ((pos (read-sequence buf in-stream)))
           (when (zerop pos) (return))
           (write-sequence buf out-stream :end pos))))))

(defun make-file-sender (filespec &optional (max-len +max-file-length+))
  "Returns a function of one argument - output byte stream. The functoin
when called writes content of the file designated by FILESPEC to the stream.

If the length of the file specified by FILESPEC <= MAX-LEN, then whole file
is written; otherwise the content is shortened to a satisfying length:
the beginning of the file, a warning message, followed by the end of the file."
  (alexandria:named-lambda file-sender (out-stream)
    (let ((file-len (test-grid-utils::file-byte-length filespec)))
      (if (<= file-len max-len)
          (send-file filespec out-stream)
          (send-shortened-file filespec out-stream file-len max-len)))))

(defun wrap-with-gzip (file-sender)
  (alexandria:named-lambda gzipping-file-sender (out-stream)
    (let ((gz-out (gzip-stream:make-gzip-output-stream out-stream)))
      (funcall file-sender gz-out)
      (finish-output gz-out))))

;; test for send-shortened-file:
;;
;; (let* ((file #P"C:\\Users\\anton\\projects\\cl-test-grid2\\test.txt")
;;        (file-len (test-grid-utils::file-byte-length file)))
;;   (with-open-file (out #P"C:\\Users\\anton\\projects\\cl-test-grid2\\test2.txt"
;;                        :direction :output
;;                        :if-does-not-exist :create
;;                        :if-exists :overwrite
;;                        :element-type '(unsigned-byte 8))
;;     (send-shortened-file file out file-len 500)))

;; old version of log submittions
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
                  ;;   (("alexandria" . (#P"/logs/alexandria.log" :filename "alexandria.log" :content-type "text/plain"))
                  ;;    ...)
                  (post-params (mapcar #'(lambda (elem)
                                           (cons (string-downcase (car elem))
                                                 (list (make-file-sender (cdr elem))
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


;; new version, can submitt to the new servlet - Upload2
(defun submit-files-impl (upload-url id-pathname-alist file-sender-factory &key (batch-size 100))
  "FILE-SENDER-FACTORY is a unary function, accepts pathname, returns
a file sender as drakma uses - another unary accepting byte output stream,
and writting the file to that stream."
  (flet ((submit-batch (id-pathname-alist-part)
           (let* (
                  ;; Now prepare POST parameters for the main submit request,
                  ;; according the drakma API for file posting.
                  ;;
                  ;; Namely, ensure the IDs are strings and add :file-name and :content-type parameters.
                  ;;
                  ;; Example: if ID-PATHNAME-ALIST is
                  ;;   ((:alexandria #P"/logs/alexandria.log") ... )
                  ;; convert it to
                  ;;   (("alexandria" . ( #'(lambda (stream) ...) :filename "alexandria.log" :content-type "text/plain"))
                  ;;    ...)
                  (post-params (mapcar #'(lambda (elem)
                                           (let ((id (car elem)) (pathname (cdr elem)))
                                             (cons (string-downcase id)
                                                   (list (funcall file-sender-factory pathname)
                                                         :filename (file-namestring pathname)
                                                         :content-type "text/plain"))))
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
           (batches (test-grid-utils::split-list id-pathname-alist batch-size))
           (response (mapcan #'submit-batch batches)))
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

;;; tests:

;; (submit-files-impl "http://5.cl-test-grid.appspot.com/upload2"
;;                    '((:test . #P"C:\\Users\\anton\\projects\\cl-test-grid2\\test.txt"))
;;                    (alexandria:compose #'wrap-with-gzip #'make-file-sender))

;; (submit-files-impl "http://5.cl-test-grid.appspot.com/upload2"
;;                    (mapcar (lambda (file)
;;                              (cons (file-namestring file) file))
;;                            (cl-fad:list-directory #P"C:\\Users\\anton\\projects\\cl-test-grid2\\test\\1"))
;;                    (alexandria:compose #'wrap-with-gzip #'make-file-sender)
;;                    :batch-size 500)

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