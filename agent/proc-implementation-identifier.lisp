;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;;
;;;; This file is loaded by agent into a separate lisp process
;;;; to query implementation identifier of that lisp.

(defpackage implementation-identifier
  (:use :cl))

(in-package :implementation-identifier)

;;; An utility function form ASDF, used in the code below.
;;; Strip out formating that is not supported on Genera.
(defmacro compatfmt (format)
  #-genera format
  #+genera
  (let ((r '(("~@<" . "")
	     ("; ~@;" . "; ")
	     ("~3i~_" . "")
	     ("~@:>" . "")
	     ("~:>" . ""))))
    (dolist (i r)
      (loop :for found = (search (car i) format) :while found :do
        (setf format (concatenate 'simple-string (subseq format 0 found)
                                  (cdr i)
                                  (subseq format (+ found (length (car i))))))))
    format))

;;; ---------------------------------------------------------------------------
;;; implementation-identifier
;;;
;;; produce a string to identify current implementation.
;;; Initially stolen from SLIME's SWANK, hacked since in ASDF.
;;; Now adopted in cl-test-grid.

(defparameter *implementation-features*
  '((:abcl :armedbear)
    (:acl :allegro)
    (:mcl :digitool) ; before clozure, so it won't get preempted by ccl
    (:ccl :clozure)
    (:corman :cormanlisp)
    (:lw :lispworks)
    :clisp :cmu :ecl :gcl :sbcl :scl :symbolics :xcl))

(defparameter *os-features*
  '((:win :windows :mswindows :win32 :mingw32) ;; shorten things on windows
    (:solaris :sunos)
    (:linux :linux-target) ;; for GCL at least, must appear before :bsd.
    (:macosx :darwin :darwin-target :apple)
    :freebsd :netbsd :openbsd :bsd
    :unix
    :genera))

(defparameter *architecture-features*
  '((:amd64 :x86-64 :x86_64 :x8664-target)
    (:x86 :i386 :i486 :i586 :i686 :pentium3 :pentium4 :pc386 :iapx386 :x8632-target)
    :hppa64
    :hppa
    (:ppc64 :ppc64-target)
    (:ppc32 :ppc32-target :ppc :powerpc)
    :sparc64
    (:sparc32 :sparc)
    (:arm :arm-target)
    (:java :java-1.4 :java-1.5 :java-1.6 :java-1.7)
    :imach))

(defun lisp-version-string ()
  (let ((s (lisp-implementation-version)))
    (declare (ignorable s))
    #+allegro (format nil
                      "~A~A~A~A"
                      excl::*common-lisp-version-number*
                      ;; ANSI vs MoDeRn - thanks to Robert Goldman and Charley Cox
                      (if (eq excl:*current-case-mode*
                              :case-sensitive-lower) "M" "A")
                      ;; Note if not using International ACL
                      ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
                      (excl:ics-target-case
                       (:-ics "8")
                       (:+ics ""))
                      (if (member :64bit *features*) "-64bit" ""))
    #+armedbear (format nil "~a-fasl~a" s system::*fasl-version*)
    #+clisp (subseq s 0 (position #\space s)) ; strip build information (date, etc.)
    #+clozure (format nil "~d.~d-f~d" ; shorten for windows
                      ccl::*openmcl-major-version*
                      ccl::*openmcl-minor-version*
                      (logand ccl::fasl-version #xFF))
    #+cmu (substitute #\- #\/ s)
    #+ecl (format nil "~A~@[-~A~]" s
                  (let ((vcs-id (ext:lisp-implementation-vcs-id)))
                    (when (>= (length vcs-id) 8)
                      (subseq vcs-id 0 8))))
    #+gcl (subseq s (1+ (position #\space s)))
    #+genera (multiple-value-bind (major minor) (sct:get-system-version "System")
               (format nil "~D.~D" major minor))
    #+lispworks (format nil "~A~@[~A~]" s
                        (when (member :lispworks-64bit *features*) "-64bit"))
    ;; #+sbcl (format nil "~a-fasl~d" s sb-fasl:+fasl-file-version+) ; f-f-v redundant w/ version
    #+mcl (subseq s 8) ; strip the leading "Version "
    #+(or cormanlisp sbcl scl) s
    #-(or allegro armedbear clisp clozure cmu cormanlisp
          ecl gcl genera lispworks mcl sbcl scl) s))

(defun first-feature (features)
  (labels
      ((fp (thing)
         (etypecase thing
           (symbol
            (let ((feature (find thing *features*)))
              (when feature (return-from fp feature))))
           ;; allows features to be lists of which the first
           ;; member is the "main name", the rest being aliases
           (cons
            (dolist (subf thing)
              (when (find subf *features*) (return-from fp (first thing))))))
         nil))
    (loop :for f :in features
      :when (fp f) :return :it)))

(defun implementation-type ()
  (first-feature *implementation-features*))

(defun implementation-identifier ()
  (labels
      ((maybe-warn (value fstring &rest args)
         (cond (value)
               (t (apply #'warn fstring args)
                  "unknown"))))
    (let ((lisp (maybe-warn (implementation-type)
                            (compatfmt "~@<No implementation feature found in ~a.~@:>")
                            *implementation-features*))
          (os   (maybe-warn (first-feature *os-features*)
                            (compatfmt "~@<No OS feature found in ~a.~@:>") *os-features*))
          (arch (or #-clisp
                    (maybe-warn (first-feature *architecture-features*)
                                (compatfmt "~@<No architecture feature found in ~a.~@:>")
                                *architecture-features*)))
          (version (maybe-warn (lisp-version-string)
                               "Don't know how to get Lisp implementation version.")))
      (substitute-if
       #\_ #'(lambda (x) (find x " /:\\(){}[]$#`'\""))
       (format nil "~(~a~@{~@[-~a~]~}~)" lisp version os arch)))))

