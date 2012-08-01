(defpackage #:stefil-api
  (:use :cl #:api-dsl)
  (:export #:run-test-suite
           #:failed-tests))

(in-package #:stefil-api)

(proclfun run-test-suite ((test-suite-name symbol)) t
  "Runs the Stefil test stuite specified by TEST-SUITE-NAME and returns 
an opaque result object. The result object may be passed to the FAILED-TESTS 
function to retreive the list of failed tests.")

(proclfun failed-tests ((test-suite-result t)) list
  "List of failed test names. Test names are downcased strings.
The TEST-SUITE-RESULT parameter must be a result of the RUN-TEST-SUITE
function.")

;; Stefil has a macro with-expected-failures, but we don't know
;; how it is intended to be used - to register "known" failues, or for some
;; other purpose. So far none of the libraries added to cl-test-grid
;; use this feature, therefore we do not distinguish the failure-description
;; having expected-p = t from other failure-descriptions.
;; 
;; When we meet the first library using with-expected-failures, we will
;; see how to support it.