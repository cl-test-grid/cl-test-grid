(defpackage #:eos-api
  (:use :cl #:api-dsl)
  (:export #:run-test-suites
           #:failed-tests))

(in-package #:eos-api)

(proclfun run-test-suites (&rest (test-suite-spec symbol)) t
  "Runs one or more Eos test suites specified by arguments and returns 
an opaque result object combining all the results. The result object may 
be passed to the FAILED-TESTS function to retreive the list of tests
failed in all the test sutes.")

(proclfun failed-tests ((test-suite-result t)) list
  "List of failed test names. Test names are downcased strings.
The TEST-SUITE-RESULT parameter must be a result of the RUN-TEST-SUITES
function.")
