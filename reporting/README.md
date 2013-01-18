Accessing the Test Results Database
===================================

All the test grid results are stored in a plain s-expression
file. It is published in a separate git repository at
https://github.com/cl-test-grid/cl-test-grid-results

The `test-grid-reporting` package contains the
reporting code we will consider below.

Do the following:

``` shell
$ git clone git@github.com:cl-test-grid/cl-test-grid.git
$ git clone git@github.com:cl-test-grid/cl-test-grid-results.git
```

``` common-lisp
CL-USER> (pushnew "cl-test-grid/" asdf:*central-registry* :test #'equal)
CL-USER> (ql:quickload :test-grid-reporting)
CL-USER> (in-package #:test-grid-reporting)
TEST-GRID-REPORTING> (defparameter *db* (test-grid-data:read-db))
```

`RESULT` Objects
==================

In most cases we found it convenient to convert
the database into a large list of `result` objects:

``` common-lisp
TEST-GRID-REPORTING> (defparameter *all-results* (list-results *db*))
```

Having a list of `result` it is easy to filter
and match them using standard Common Lisp functions.

The most interesting properties of the `result` objects
are accessed using the following functions:

- `(result-spec result)` Result description in one of the following forms:
  - `(:load "some-asdf-system-name" [:ok | :fail | :crash | :timeout])`
    Returned if the result object represents result of loading an ASDF system.
    Here the notation `[:ok | :fail | :crash | :timeout]` shoud be read as "one of `:ok`, `:fail`, `:crash` or `:timeout`".
    
    `:ok` means the load operation succeeded;

    `:fail` means the load operation failed;

    `:crash` means the child lisp process loading asdf
             system exited without returning a result;            

    `:timeout` means that the child lisp process
               hasn't finished in a specified timeout time.

  - =(:test-case "some-test-case-name" [:fail | :known-fail | :unexpected-ok])= ::
    Represents abnormal result of a single test case.
    - =:fail= :: means the test case has failed;
    - =:known-fail= :: also means that the test case has failed, but
                       the library developers have this testcase
                       marked as a known failure (known failures is a feature
                       provided by some test frameworks; it may be used
                       by the developers, for example, to mark
                       test cases which are impossible to fix right now,
                       maybe due to lack of support or bugs
                       in CL implemntatons, 3rd party libraries, or similar);
    - =:unexpected-ok= :: means the testcase marked as known failure has not failed.
  - =(:whole-test-suite [:ok | :no-resource | :fail | :crash | :timeout])= ::
       Result for a whole test suite.
       - =:ok= :: None of the testcases has failed.
       - =:fail= :: Either some test cases failed but the test
            framework does not allow to distinguish
            particular test case, or some problem
            prevented the test suite from running at all.
            Example of such a problem may be that the
            testsuite or one of it's dependencies
            doesn't compile/load due to errors
            in lisp code; or absense of necessary
            foreign library on the test system.
       - =:no-resource= :: This status is designed to represent
                           situations when testsuite can not be run due
                           to absense of necessary enviromnent.

                           For example, CFFI test suite needs a small
                           C library to be build. On Windows user must
                           do it manually. If this library is not found,
                           testgrid adapter of the CFFI test suite returns :no-resource.
                           
                           Or, external-program test suite can only be
                           run on *nix platforms. On Windows testgrid
                           adapter returns :no-resource.

                           The :no-resource handling in testsuite adapters
                           is optional, as every testsuite may have different
                           requirements.

                           Today, most testsuite adapters in testgrid
                           do not implemente such a handling, and
                           in case of any problems when running
                           the tests :fail is recorded.
       - =:crash= :: means the child lisp process running the test suite
                     exited without returning a result;
       - =:timeout= :: means that the child lisp process
                       hasn't finished in a specified timeout time.
- =(libname result)= :: Name of the library tested - a keyword, like :babel, :alexandria, etc.

- =(lisp result)= :: Lisp implementation identifier - a string, for example "clisp-2.49-unix-x86_64",
   "cmu-20c_release-20c__20c_unicode_-linux-x86", "sbcl-1.0.54-linux-x64"

- =(lib-world result)= :: A string naming the set of libraries and their versions used during testing,
  for example "quicklisp 2012-07-03", "quicklisp 2012-08-11".

- =(system-name result)= :: If the result descibes ASDF system load result, then the
  name of that ASDF system - a string, like "arnesi", "anaphora".
  Otherwize =NIL=.

- =(log-uri result)= :: URI of the stored online output produced by the child lisp process
  performed the test suite or tested the ASDF system load.

#+END_SRC common-lisp
