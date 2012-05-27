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

;; copy/paste from
;; http://www.gigamonkeys.com/book/practical-an-mp3-browser.html
(defmacro with-safe-io-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*read-eval* nil))
       ,@body)))

(defun safe-read (&rest args)
  (with-safe-io-syntax (apply #'read args)))

(defun file-byte-length (path)
  (with-open-file (s path
                     :direction :input
                     :element-type '(unsigned-byte 8))
    (file-length s)))

(defconstant +max-file-length+ 100000)

(defun limit-file-length (filespec &optional (max-len +max-file-length+))
  "If the length of the file specified by FILESPEC is satisfying,
returns the FILESPEC; otherwise returns a function accepting a binary
stream as it's only argument, and writing to that stream the file content
shortened to a satisfying length: the beginning of the file,
a warning message, followed by the end of the file."
  (let ((file-len (file-byte-length filespec)))
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

;;; Debugging tools

(defun log-dir ()
  (merge-pathnames "upload-debug-logs/"
                   (asdf:system-source-directory '#:test-grid-blobstore)))

(defun log-file ()
  (merge-pathnames "upload.log" (log-dir)))

(defun with-log-stream-impl (body-func)
  (ensure-directories-exist (log-dir))
  (with-open-file (out (log-file)
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (funcall body-func out)))

(defmacro with-log-stream ((var) &body body)
  `(with-log-stream-impl (lambda (,var)
                           ,@body)))

(defparameter *upload-id-random-state* (make-random-state t))
(defun new-upload-id ()
  (random 1000000 *upload-id-random-state*))

(defun fmt-log-time (universal-time &optional destination)
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time 0)
    (funcall #'format
             destination
             "~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
             year month date hour min sec)))

(defun log-time ()
  (fmt-log-time (get-universal-time)))

;;; end of debugging

(defmethod test-grid-blobstore:submit-files ((blobstore blobstore) id-pathname-alist)
  (with-log-stream (lg)
    (let ((drakma:*header-stream* lg))
      (format lg "~A ->test-grid-blobstore:submit-files~%" (log-time))
      (let* ( ;; Google App Engine does not allow to submit blobs to a constant URL,
             ;; we need to perform a separate request to our servlet, which will
             ;; generate an URL where we can upload files.
             (upload-url (drakma:http-request (format nil "~A/upload-url" (base-url blobstore))
                                              :content-type "text/text"))
             ;; Now prepare POST parameters for the main submit request,
             ;; according the drakma API for file posting.
             ;;
             ;; Namely, ensure the IDs are strings and add "text/plain" content type.
             ;;
             ;; Example: if ID-PATHNAME-ALIST is
             ;;   ((:alexandria #P"/logs/alexandria.log") ... )
             ;; convert it to
             ;;   (("alexandria" #P"/logs/alexandria.log" :content-type "text/plain") ... )
             (post-params (progn
                            (format lg "~A upload-url: ~A~%" (log-time) upload-url)
                            (mapcar #'(lambda (elem)
                                        (cons (string-downcase (car elem))
                                              (list (limit-file-length (cdr elem))
                                                    :filename (file-namestring (cdr elem))
                                                    :content-type "text/plain")))
                                    id-pathname-alist)))
             ;; Perrorm the query.
             (response (with-open-stream (in (drakma:http-request upload-url
                                                                  :method :post
                                                                  :content-length t
                                                                  :parameters post-params
                                                                  :want-stream t))
                         ;; And read the response
                         (safe-read in))))
        (format lg "~A response received from blobstore: ~S~%" (log-time) response)
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
        (let ((result
               (flet ((get-blobkey (for-id)
                        (or (cdr (assoc for-id response :test #'string-equal))
                            (error "The response does not contain a blobkey for the ~A" for-id))))
                 (mapcar (lambda (id-pathname-pair)
                           (cons (car id-pathname-pair)
                                 (get-blobkey (car id-pathname-pair))))
                         id-pathname-alist))))
          (format lg "~A <-test-grid-blobstore:submit-files: ~S~%" (log-time) result)
          result)))))

(defmethod test-grid-blobstore:submit-run-info ((blobstore blobstore) run-info)
  (assert (not (null run-info)))
  (let ((response (drakma:http-request (format nil "~A/submit-run-info" (base-url blobstore))
                                       :method :post
                                       :parameters `(("run-info" . ,(prin1-to-string run-info))))))
    (when (not (eq :ok (with-input-from-string (s response)
                         (safe-read s))))
      (error "Error submitting run info to the server. Unexpected response: ~A." response))))
