;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

(defpackage #:test-grid-gae-blobstore
  (:nicknames #:tg-gae-blobstore)
  (:use :cl)
  (:export #:make-blob-store
           #:submit-files
           #:submit-files2
           #:submit-run-info
           #:send-notification
           #:delete-blobs))

(in-package #:test-grid-gae-blobstore)

(defgeneric submit-files (blobstore id-pathname-alist)
  (:documentation
   "Submits the files to the blobstore. Returns blob keys
for every submitted file (the blobkey is used for later
references to the file in the blobstore).

Example:
 (submit-files my-blobstore '((:flexi-streams . #P\"flexi-streams.log\")
                              (:cl-ppcre . #P\"cl-ppcre.log\")))
  => ((:flexi-streams . \"nFeUku39YtilF6s8zkXTlg\")
      (:cl-ppcre . \"nG9B8tHEquLEPhsL3t8wcA\"))

The ID in the ID-PATHNAME-ALIST may be either a string or a symbol,
and must be unique when compared case insensitively.

If the function returns without errors, it is guaranteed
that all the files are submitted and the return value
has a blobkey for every file.

Signals an ERROR in case of problems."))

(defgeneric submit-run-info (blobstore run-info)
  (:documentation
   "Submits test run result RUN-INFO (a lisp object)
to central database."))

(defgeneric send-notification (blobstore subject body)
  (:documentation
   "Sends notification http://groups.google.com/group/cl-test-grid-notifications."))

;;; Implementation

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

(defun submit-files-impl (upload-url-fn id-pathname-alist file-sender-factory &key (batch-size 100))
  "FILE-SENDER-FACTORY is a unary function, accepts pathname, returns
a file sender as drakma uses - another unary accepting byte output stream,
and writting the file to that stream."
  (let ((total (length id-pathname-alist))
        (done 0))
    (labels ((submit-batch (id-pathname-alist-part)
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
                     (drakma:http-request (funcall upload-url-fn)
                                          :method :post
                                          :content-length t
                                          :parameters post-params
                                          :want-stream t)
                   (declare (ignore headers uri stream2 must-close))
                   (cond ((/= 200 status-code)
                          (error "Error uploading files, the HTTP response code ~A: ~A" status-code reason-phrase))
                         (t (with-open-stream (stream stream)
                              ;; And read the response
                              (test-grid-utils::safe-read stream)))))))
             (submit-batch-logging (id-pathname-alist-part)
               (prog1
                   (submit-batch id-pathname-alist-part)
                 (incf done (length id-pathname-alist-part))
                 (log:info "~A/~A files uploaded" done total))))
      (let* ( ;; Split the files submitted into batches by < 500 elements
             ;; to workaround GAE blobstore issue: https://code.google.com/p/googleappengine/issues/detail?id=8032
             (batches (test-grid-utils::split-list id-pathname-alist batch-size))
             (response (mapcan #'submit-batch-logging batches)))
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
                  id-pathname-alist))))))

(defun submit-files2 (blobstore id-pathname-alist &key (batch-size 300) keep-names-p)
  "KEEP-NAMES-P when true make the log stored under original FILE-NAMESTRING
of the pathnames passed in ID-PATHNAME-ALIST."
  (submit-files-impl (constantly (format nil "~a/upload2~:[~;?keepNames=true~]"
                                         (base-url blobstore)
                                         keep-names-p))
                     id-pathname-alist
                     (alexandria:compose #'wrap-with-gzip #'make-file-sender)
                     :batch-size batch-size))

;; old version of log submittion
(defmethod submit-files ((blobstore blobstore) id-pathname-alist)
  ;; Google App Engine Blobstore does not allow to submit blobs to a constant URL,
  ;; we need to perform a separate request to our servlet, which will
  ;; generate an URL where we can upload files.
  ;; https://developers.google.com/appengine/docs/java/blobstore/overview
  (flet ((new-upload-url ()
           (drakma:http-request (format nil "~A/upload-url" (base-url blobstore))
                                :content-type "text/plain")))
    (submit-files-impl #'new-upload-url id-pathname-alist #'make-file-sender)))

;;; tests:

;; (submit-files2 (make-blob-store :base-url "http://7.cl-test-grid.appspot.com")
;;                '((:test . #P"C:\\Users\\anton\\projects\\cl-test-grid2\\work\\test.txt")))

;; (submit-files (make-blob-store :base-url "http://7.cl-test-grid.appspot.com")
;;               '((:test . #P"C:\\Users\\anton\\projects\\cl-test-grid2\\work\\test.txt")))

;; (submit-files2 (make-blob-store :base-url "http://7.cl-test-grid.appspot.com")
;;                (mapcar (lambda (file)
;;                          (cons (file-namestring file) file))
;;                        (cl-fad:list-directory #P"C:\\Users\\anton\\projects\\cl-test-grid2\\work\\test\\1"))
;;                :batch-size 500)

(defmethod submit-run-info ((blobstore blobstore) run-info)
  (assert (not (null run-info)))
  (let ((response (drakma:http-request (format nil "~A/submit-run-info" (base-url blobstore))
                                       :method :post
                                       :parameters `(("run-info" . ,(prin1-to-string run-info))))))
    (when (not (eq :ok (with-input-from-string (s response)
                         (test-grid-utils::safe-read s))))
      (error "Error submitting run info to the server. Unexpected response: ~A." response))))

(defmethod send-notification ((blobstore blobstore) subject body)
  (assert (not (null subject)))
  (setf body (or body ""))
  (let ((response (drakma:http-request (format nil "~A/send-notification" (base-url blobstore))
                                       :method :post
                                       :parameters `(("subject" . ,subject)
                                                     ("body" . ,body)))))
    (when (not (eq :ok (with-input-from-string (s response)
                         (test-grid-utils::safe-read s))))
      (error "Error sending message to admin. Unexpected response: ~A." response))))

;; Blob keys referred via http://cl-test-grid.appspot.com/blob?key=<blob key>
;; in Internet.
;; Collected manually via google search at 2013.02.13
(defparameter *logs-to-keep*
  '("100025" "1037868" "1044920" "167051" "167058" "170032" "171015" "176038"
    "177044" "178062" "180050" "187096" "190086" "197101" "198057" "198076"
    "216013" "226017" "260299" "269153" "274297" "297008" "340364" "357430"
    "370563" "374954" "386272" "388372" "389242" "430179" "498392" "502394"
    "534856" "564037" "564037" "599867" "600015" "607011" "608020" "6100" "647037"
    "647056" "648045" "7018" "789714" "792731" "854116" "860701" "882267" "941904"
    "AMIfv95LxbkgWChpsuKddRA8bTozHhRzkT8zH7wqG_e6E84kTL58N5duGsXDkKDTthXmv7QjDAJtGxnRFshqkUKk9L59UvGg7luWEjxfEpPqcq9mNLAdus1SMdmQewx0_TUrkyOEX-TAV76U2LZX_i9qfa-q0RerOQ"
    "AMIfv96z3zpAJ7NBVUox0ZXqXE0MofFZLcuaXtttHS6VFsXVRDyJqCFVZRfdNYiJqAOZbRbnUvIfyUzsK9t-qRBQmGtwyaVd7WcGMTa0XaKEAEY1AL1NRz6SkwlB6mV-2utmWKkmHQC94iVpGHbaZ2ZifpGBjtp_Hg"
    "AMIfv97DXhtnmey3lh8C3P-s6SbAFyFqEFodohqccgB7UtGbKn7Qg8jlH5RanWIiC-2recfsNeWoPTbsZC1SLsjLszHLja2b3yv25TNJ_6p45mDYOWVuPIWgDkuVOdBCNGuSgSsMGjRMeLorD0fxOWwK2qPOBmZajA"
    "AMIfv97suboJpeei-uBWzlkqcR7CTlyh0Izhvi7u_29HNBgu80ScYf0Mj6zWPjgbsosA-F0Q12HP8o9S5zhsEelTfss8_3C7sjgcuG_q_grR-jMfXPLLRzu6CNytLoNk23rwqlQ6AsajxTRYFubFbz3iBWl5uo8iZQ"
    "AMIfv97wo4FQxGLBZOagyZyLZCqwMWavAfwsByKxjq8QiJQ5rIzEggGwGJ_kH2qRZLMb8N_el8aKIpLDbnr67Pxcy9r8RFKmBnjTQ1B44yaCcyZWtO2CSbBliyAINvoI41_R8uA8hoPia-yXPdlmADiJcavCCgpHGA"))

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
