;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.
;;;;
;;;; This file is loaded by agent into a separate lisp process
;;;; to update quicklisp.

(let* ((this-file (load-time-value (or *load-truename* #.*compile-file-pathname*)))
       (this-file-dir (make-pathname :name nil :type nil :defaults this-file)))
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

(defun restarting-downloads-impl (body-fn)
  (let ((restart-counters (make-hash-table :test #'equal)))
    (handler-bind
        ((error (lambda (e)
                  (when (typep e (read-from-string "ql-dist:badly-sized-local-archive"))
                    (let* ((release (fncall "ql-dist::invalid-local-archive-release" e))
                           (retry-count (incf (gethash release
                                                       restart-counters
                                                       0))))
                      (when (<= retry-count 3)
                        (format t "~A download retry #~A~%" release retry-count)
                        (invoke-restart (read-from-string "ql-dist:delete-and-retry"))))))))
      (funcall body-fn))))

(defmacro restarting-downloads (&body body)
  `(restarting-downloads-impl (lambda () ,@body)))

(defun do-quicklisp-update (install-dir dist-specifier)
  ;; returns QL-DIST:DIST, it can be out of sync with FS data,
  ;; becuase ql:update-dist doesn't update the DIST object, but only updates FS data
  (restarting-downloads

    (install-quicklisp install-dir)
    (fncall "ql:update-client" :prompt nil)

    (cond ((string= "quicklisp" dist-specifier)
           (let ((qlalpha (fncall "ql-dist:find-dist" "qlalpha")))
             (when qlalpha (fncall "ql-dist:disable" qlalpha)))
           (let ((dist (fncall "ql-dist:dist" "quicklisp")))
             (fncall "ql-dist:enable" dist)
             (fncall "ql:update-dist" dist :prompt nil)
             dist))
          ((string= "qlalpha" dist-specifier)
           (fncall "ql-dist:disable" (fncall "ql-dist:dist" "quicklisp"))
           (let ((dist (or (fncall "ql-dist:find-dist" "qlalpha")
                           (fncall "ql-dist:install-dist" "http://alpha.quicklisp.org/dist/qlalpha.txt" :prompt nil :replace t))))
             (fncall "ql-dist:enable" dist)
             (fncall "ql:update-dist" dist :prompt nil)
             dist))
          (t
           ;; dist-specifier must be an URL string, like
           ;; "http://beta.quicklisp.org/dist/quicklisp/2012-08-11/distinfo.txt"
           ;; Useful, when we need to test a particular quicklisp version.
           (fncall "ql-dist:install-dist" dist-specifier :replace t :prompt nil))
          ;; Note, even if other dists are installed in quicklisp,
          ;; the new dist gets higher precedence by ql:quickload, because
          ;; ql-dist:install-dist; records (get-universal-time) as the "preference number" of the dist,
          ;; thus giving the dist higher preference than of previously installed dists.
          ;;
          ;; It would be more reliable to explicitly disable other dists,
          ;; but quicklisp does not provide a function to enumerate dists.
          )))

(defun update-quicklisp (install-dir dist-specifier log-file)
  (labels ((fmt-time (universal-time &optional destination)
             (multiple-value-bind (sec min hour date month year)
                 (decode-universal-time universal-time 0)
               (format destination
                       "~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                       year month date hour min sec)))
           (log-msg (destination msg)
             (format destination "~&; ~A ~A~%" (fmt-time (get-universal-time)) msg))
           (log-hdr (destination msg)
             (format destination "~&;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
             (log-msg destination msg)
             (format destination "~&;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%"))
           (dist-clean (dist)
             ;; returns the DIST
             (handler-case
                 (progn
                   (log-msg t "running (ql-dist:clean <dist>)...")
                   (fncall "ql-dist:clean" dist)
                   (log-msg t "finished (ql-dist:clean <dist>)")
                   dist)
               (serious-condition (e)
                 (warn "problem runnining ql-dist:clean: ~A: ~A"
                       (let ((*package* (find-package :keyword))) (princ-to-string (type-of e)))
                       e)
                 dist)))
           (fresh-dist (dist)
             ;; find a fresh DIST object in case it is stale,
             ;; because QL:UPDATE-DIST only changes data on file system
             ;; and does not update the DIST object
             (fncall "ql-dist:dist" (fncall "ql-dist:name" dist)))
           (version-string (dist)
             (format nil "~A ~A"
                     (fncall "ql-dist:name" dist)
                     (fncall "ql-dist:version" dist))))
    (capturing-io log-file
                  (lambda ()
                    (prog2
                        (log-hdr t "update-quicklisp start")
                        (version-string (dist-clean (fresh-dist (do-quicklisp-update install-dir
                                                                                     dist-specifier))))
                      (log-hdr t "update-quicklisp done"))))))
