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

(defmethod test-grid-blobstore:submit-files ((blobstore blobstore) id-pathname-alist)
  (let* (;; Google App Engine does not allow to submit blobs to a constant URL,
         ;; we need to perform a separate request to our servlet, which will
         ;; generate an URL where we can upload files.
         (upload-url (drakma:http-request (format nil "~A/upload-url" (base-url blobstore)) 
                                          :content-type "text/text"))
         
         ;; Now prepare POST parameters for the main submit request,
         ;; according the drakma API for file posting.
         ;;
         ;; Namely ensure the IDs are strings and add "text/plain" content type. 
         ;;
         ;; Example: if we had 
         ;;   ((:flexi-streams #P"/logs/flexi-stream.log") ... )
         ;; convert it to 
         ;;   (("flexi-streams" #P"/logs/flexi-stream.log" :content-type "text/plain") ... )
         (post-params (mapcar #'(lambda (elem) 
                                  (cons (string-downcase (car elem))
                                        (list (cdr elem)
                                              :content-type "text/plain")))
                              id-pathname-alist))
         ;; Perrorm the query.
         (response (with-open-stream (in (drakma:http-request upload-url
                                                              :method :post
                                                              :content-length t                         
                                                              :parameters post-params
                                                              :want-stream t))
                     ;; And read the response
                     (safe-read in))))
    ;; Now RESPONSE contains an alist of 
    ;; (<stringified ID> . <blob key>)  pairs. 
    ;; For example:
    ;;    (("flexi-streams" . "cJVA1Klp7o-Lz2Cc6KuPcg") ...)
    ;; As in the original id-pathname-alist the IDs might be represented
    ;; as symbols, lets return response with IDs in the original form, e.g.
    ;;    ((:flexi-streams . "cJVA1Klp7o-Lz2Cc6KuPcg") ...)
    ;;
    ;; During the conversion we also check that we got blobkeys for
    ;; all the files we submitted.
    (flet ((get-blobkey (for-id)
             (or (cdr (assoc for-id response :test #'string-equal))                                            
                 (error "The response does not contain a blobkey for the ~A" for-id))))
      (mapcar (lambda (id-pathname-pair)
                (cons (car id-pathname-pair)
                      (get-blobkey (car id-pathname-pair))))
              id-pathname-alist))))

(defmethod test-grid-blobstore:submit-run-info ((blobstore blobstore) run-info)
  (let ((response (drakma:http-request (format nil "~A/submit-run-info" (base-url blobstore))
                                       :method :post
                                       :parameters `(("run-info" . ,(prin1-to-string run-info))))))
    (when (not (eq :ok (with-input-from-string (s response)
                         (safe-read s))))
      (error "Error submitting run info to the server. Unexpected response: ~A." response))))
