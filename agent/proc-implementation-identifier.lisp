;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;;
;;;; This file is loaded by agent into a separate lisp process
;;;; to query implementation identifier of that lisp.

(defpackage implementation-identifier
  (:use :cl))

(in-package :implementation-identifier)

;;; An utility function from ASDF used by the code below

(defun featurep (x &optional (features *features*))
  (cond
    ((atom x)
     (and (member x features) t))
    ((eq :not (car x))
     (assert (null (cddr x)))
     (not (featurep (cadr x) features)))
    ((eq :or (car x))
     (some #'(lambda (x) (featurep x features)) (cdr x)))
    ((eq :and (car x))
     (every #'(lambda (x) (featurep x features)) (cdr x)))
    (t
     (error "Malformed feature specification ~S" x))))


;;; The code below is copy/pasted from ASDF.
;;; ASDF version:
;;; commit ef15f1f1ab4a1c627a9a6eaa83c9587478a0adde
;;; Author: Francois-Rene Rideau <tunes@google.com>
;;; Date:   Thu Nov 7 10:48:55 2013 -0500

;;;; implementation-identifier
;;
;; produce a string to identify current implementation.
;; Initially stolen from SLIME's SWANK, completely rewritten since.
;; We're back to runtime checking, for the sake of e.g. ABCL.

(defun first-feature (feature-sets)
  "A helper for various feature detection functions"
  (dolist (x feature-sets)
    (multiple-value-bind (short long feature-expr)
        (if (consp x)
            (values (first x) (second x) (cons :or (rest x)))
            (values x x x))
      (when (featurep feature-expr)
        (return (values short long))))))

(defun implementation-type ()
  "The type of Lisp implementation used, as a short UIOP-standardized keyword"
  (first-feature
   '(:abcl (:acl :allegro) (:ccl :clozure) :clisp (:corman :cormanlisp)
     (:cmu :cmucl :cmu) :ecl :gcl
     (:lwpe :lispworks-personal-edition) (:lw :lispworks)
     :mcl :mkcl :sbcl :scl (:smbx :symbolics) :xcl)))

(defvar *implementation-type* (implementation-type)
  "The type of Lisp implementation used, as a short UIOP-standardized keyword")

(defun operating-system ()
  "The operating system of the current host"
  (first-feature
   '(:cygwin (:win :windows :mswindows :win32 :mingw32) ;; try cygwin first!
     (:linux :linux :linux-target) ;; for GCL at least, must appear before :bsd
     (:macosx :macosx :darwin :darwin-target :apple) ; also before :bsd
     (:solaris :solaris :sunos) (:bsd :bsd :freebsd :netbsd :openbsd) :unix
     :genera)))

(defun architecture ()
  "The CPU architecture of the current host"
  (first-feature
   '((:x64 :x86-64 :x86_64 :x8664-target :amd64 (:and :word-size=64 :pc386))
     (:x86 :x86 :i386 :i486 :i586 :i686 :pentium3 :pentium4 :pc386 :iapx386 :x8632-target)
     (:ppc64 :ppc64 :ppc64-target) (:ppc32 :ppc32 :ppc32-target :ppc :powerpc)
     :hppa64 :hppa :sparc64 (:sparc32 :sparc32 :sparc)
     :mipsel :mipseb :mips :alpha (:arm :arm :arm-target) :imach
     ;; Java comes last: if someone uses C via CFFI or otherwise JNA or JNI,
     ;; we may have to segregate the code still by architecture.
     (:java :java :java-1.4 :java-1.5 :java-1.6 :java-1.7))))

#+clozure
(defun ccl-fasl-version ()
  ;; the fasl version is target-dependent from CCL 1.8 on.
  (or (let ((s 'ccl::target-fasl-version))
        (and (fboundp s) (funcall s)))
      (and (boundp 'ccl::fasl-version)
           (symbol-value 'ccl::fasl-version))
      (error "Can't determine fasl version.")))

(defun lisp-version-string ()
  "return a string that identifies the current Lisp implementation version"
  (let ((s (lisp-implementation-version)))
    (car ; as opposed to OR, this idiom prevents some unreachable code warning
     (list
      #+allegro
      (format nil "~A~@[~A~]~@[~A~]~@[~A~]"
              excl::*common-lisp-version-number*
              ;; M means "modern", as opposed to ANSI-compatible mode (which I consider default)
              (and (eq excl:*current-case-mode* :case-sensitive-lower) "M")
              ;; Note if not using International ACL
              ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
              (excl:ics-target-case (:-ics "8"))
              (and (member :smp *features*) "S"))
      #+armedbear (format nil "~a-fasl~a" s system::*fasl-version*)
      #+clisp
      (subseq s 0 (position #\space s)) ; strip build information (date, etc.)
      #+clozure
      (format nil "~d.~d~@[-~a~]~@[-r~a~]-f~d" ; shorten for windows
              ccl::*openmcl-major-version*
              ccl::*openmcl-minor-version*
              ;; my: ----------------------
              ;; Include more info;
              ;; based on CCL's code of lisp-implementation-version.
              ;; The format string above is adjusted accordingly.
              (unless (null ccl::*openmcl-revision*)
                ccl::*openmcl-revision*)
              (if (and (typep ccl::*openmcl-svn-revision* 'string)
                       (> (length ccl::*openmcl-svn-revision*) 0))
                  ccl::*openmcl-svn-revision*)
              ;; end my -------------------
              (logand (ccl-fasl-version) #xFF))
      #+cmu (substitute #\- #\/ s)
      #+scl (format nil "~A~A" s
                    ;; ANSI upper case vs lower case.
                    (ecase ext:*case-mode* (:upper "") (:lower "l")))
      #+ecl (format nil "~A~@[-~A~]" s
                    (let ((vcs-id (ext:lisp-implementation-vcs-id)))
                      (subseq vcs-id 0 (min (length vcs-id) 8))))
      #+gcl (subseq s (1+ (position #\space s)))
      #+genera
      (multiple-value-bind (major minor) (sct:get-system-version "System")
        (format nil "~D.~D" major minor))
      #+mcl (subseq s 8) ; strip the leading "Version "
      s))))

(defun implementation-identifier ()
  "Return a string that identifies the ABI of the current implementation,
suitable for use as a directory name to segregate Lisp FASLs, C dynamic libraries, etc."
  (substitute-if
   #\_ #'(lambda (x) (find x " /:;&^\\|?<>(){}[]$#`'\""))
   (format nil "~(~a~@{~@[-~a~]~}~)"
           (or (implementation-type) (lisp-implementation-type))
           (or (lisp-version-string) (lisp-implementation-version))
           (or (operating-system) (software-type))
           (or (architecture) (machine-type)))))


