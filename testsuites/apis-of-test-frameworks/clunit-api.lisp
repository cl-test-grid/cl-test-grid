(defpackage #:clunit-api
  (:use :cl #:api-dsl)
  (:export #:run-test-suite
           #:failed-tests))

(in-package #:clunit-api)

(proclfun run-test-suite ((test-suite-name symbol)) t
  "Runs the clunit test suite specified by TEST-SUITE-NAME and
returns an opaque result object. The result object may be passed
to the FAILED-TESTS function to retreive the list of failed tests.")

(proclfun failed-tests ((test-suite-result t)) list
  "List of failed test names. Test names are downcased strings.
The TEST-SUITE-RESULT parameter must be a result of the RUN-TEST-SUITE
function.")
