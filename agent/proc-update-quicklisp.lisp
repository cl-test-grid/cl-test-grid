;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; This file is loaded by agent into a separate lisp process
;;;; to update quicklisp.

(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :directory (pathname-directory this-file))))
  (load (merge-pathnames "quicklisp.lisp" this-file-dir)))

(defun install-quicklisp (install-dir)
  (handler-bind ((error #'(lambda (err)
                            (declare (ignore err))
                            (when (find-restart 'quicklisp-quickstart::load-setup)
                              (invoke-restart 'quicklisp-quickstart::load-setup)))))
    (quicklisp-quickstart:install :path install-dir))

  (let ((asdf-init-file (merge-pathnames "asdf-config/init.lisp"
                                         install-dir)))
    (ensure-directories-exist asdf-init-file)
    (with-open-file (f asdf-init-file
                     :direction :output
                     :element-type 'character
                     :if-does-not-exist :create
                     :if-exists :supersede)
      (write-string "(asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration))"
                    f)
      (terpri f))))

(defmacro fncall (funname &rest args)
  `(funcall (read-from-string ,funname) ,@args))

(defun do-quicklisp-update (install-dir)
  (install-quicklisp install-dir)
  (fncall "quicklisp:update-client" :prompt nil)

  (flet ((version-string (dist)
           ;; find a fresh DIST object in case it is stale,
           ;; because QL:UPDATE-DIST only changes data on file system
           ;; and does not update the DIST object
           (let ((dist (fncall "ql-dist:dist" (fncall "ql-dist:name" dist))))
             (format nil "~A ~A"
                     (fncall "ql-dist:name" dist)
                     (fncall "ql-dist:version" dist)))))

    ;; Update and use the "quicklisp" dist:
    (let ((qlalpha (fncall "ql-dist:find-dist" "qlalpha")))
      (when qlalpha (fncall "ql-dist:disable" qlalpha)))
    (let ((dist (fncall "ql-dist:dist" "quicklisp")))
      (fncall "ql-dist:enable" dist)
      (fncall "ql:update-dist" dist :prompt nil)
      (version-string dist))
    ;; or, if we need to install particular quicklisp version:
    ;;(version-string (fncall "ql-dist:install-dist" "http://beta.quicklisp.org/dist/quicklisp/2012-08-11/distinfo.txt" :replace t :prompt nil))

    ;; Update and use the "qlalpha" dist:
    ;;(fncall "ql-dist:disable" (fncall "ql-dist:dist" "quicklisp"))
    ;;(let ((dist (fncall "ql-dist:install-dist" "http://alpha.quicklisp.org/dist/qlalpha.txt" :prompt nil :replace t)))
    ;;  (fncall "ql-dist:enable" dist)
    ;;  (fncall "ql:update-dist" dist :prompt nil)
    ;;  (version-string dist)
    ))

(defun update-quicklisp (install-dir log-file)
  (labels ((fmt-time (universal-time &optional destination)
             (multiple-value-bind (sec min hour date month year)
                 (decode-universal-time universal-time 0)
               (format destination
                       "~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                       year month date hour min sec)))
           (log-msg (destination msg)
             (format destination "~&;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
             (format destination "; ~A ~A" (fmt-time (get-universal-time)) msg)
             (format destination "~&;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")))
    (saving-output log-file
                   (lambda ()
                     (prog2
                         (log-msg t "update-quicklisp start")
                         (do-quicklisp-update install-dir)
                       (log-msg t "update-quicklisp done"))))))
