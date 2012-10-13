(defpackage #:test-grid-blobstore
  (:use :cl)
  (:export #:submit-files
           #:submit-run-info
           #:tell-admin))

(in-package #:test-grid-blobstore)

(defgeneric submit-files (blobstore id-pathname-alist)
  (:documentation
   "Submits the files to the blobstore. Returns blob keys
for every submitted file (the blobkey is used for later
references to the file in the blobstore).

Example:
 (submit-files my-blobstore '((:flexi-streams . #P\"flexi-streams.log\")
                              (:cl-ppcre . #P\"cl-ppcre.log\")))
  => ((:flexi-streams . \"nFeUku39YtilF6s8zkXTlg\")
      (:cl-ppcre . \"nG9B8tHEquLEPhsL3t8wcA\"))

The ID in the ID-PATHNAME-ALIST may be either a string or a symbol,
and must be unique when compared case insensitively.

If the function returns without errors, it is guaranteed
that all the files are submitted and the return value
has a blobkey for every file.

Signals an ERROR in case of problems."))

(defgeneric submit-run-info (blobstore run-info)
  "Submits test run result RUN-INFO (a lisp object)
to central database.")

(defgeneric tell-admin (blobstore subject body)
  "Sends message to admin.")
