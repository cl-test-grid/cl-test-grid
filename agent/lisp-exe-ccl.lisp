(defpackage #:lisp-exe-ccl
  (:use :cl))

(in-package #:lisp-exe-ccl)

;; On Windows CCL api returns process handle,
;; but not ID. To retrieve process ID by handle
;; WinAPI functio GetProcesID may be used.
;;
;; CCL ticket: http://trac.clozure.com/ccl/ticket/983.
;;
;; We keep it in a separate file becase CCL uses
;; #_ reader macro, which is absent in other lisps
;; and confuses their readers (ECL and CLISP have
;; problem with id).
(defun win-process-handle-to-id (process-handle)
  #+windows
  (#_GetProcessId process-handle))

