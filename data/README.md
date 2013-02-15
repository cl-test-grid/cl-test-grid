Test grid resul objects are represented by property lists.
Example of test results database:

``` common-lisp
(:schema 6
 :runs ((:descr (:lisp "sbcl-1.1.3-linux-x86" :lib-world "quicklisp 2012-12-23" :time 3567905766 :run-duration 4856 :contact-email "avodonosov@yandex.ru")
         :results ((:libname :3B-SWF
                    :load-results ((:system "3b-swf" :status :OK :log-blob-key "1iew8cc0h0" :log-byte-length 2096 :load-duration 1003/250)
                                   (:system "3b-swf-swc" :status :OK :log-blob-key "67ta62onfu" :log-byte-length 4644 :load-duration 5807/100)))
                   (:libname :ALEXANDRIA :status (:failed-tests () :known-to-fail ()) :log-blob-key "1cuq4v7hcr" :log-byte-length 22128 :test-duration 3013/1000
                    :load-results ((:system "alexandria" :status :OK :log-blob-key "11of4il7dv" :log-byte-length 1707 :load-duration 126/125)
                                   (:system "alexandria-tests" :status :OK :log-blob-key "1ejuor0bx2" :log-byte-length 1764 :load-duration 1013/1000)))
                   (:libname :BABEL :status :FAIL :log-blob-key "kn5t6pp3sk" :log-byte-length 4172 :test-duration 1011/1000
                    :load-results ((:system "babel" :status :OK :log-blob-key "fg0ccuo4z3" :log-byte-length 1682 :load-duration 101/100)
                                   (:system "babel-streams" :status :OK :log-blob-key "gqc20yclat" :log-byte-length 1748 :load-duration 2009/1000)
                                   (:system "babel-tests" :status :FAIL :log-blob-key "19jvlebgry" :log-byte-length 4181 :load-duration 101/100)))))
        (:descr (:lisp "sbcl-1.1.3-macosx-x64" :lib-world "quicklisp 2012-12-23" :time 3567091719 :run-duration 10461 :contact-email "pnathan@vandals.uidaho.edu")
         :results ((:libname :BABEL :status (:failed-tests ("babel-tests.encoder-and-decoder-return-values.encoder/decoder-retvals" "babel-tests.utf-8b.2") :known-to-fail ()) :log-blob-key "1094499" :log-byte-length 5466 :test-duration 1003/250
                    :load-results ((:system "babel" :status :OK :log-blob-key "1086496" :log-byte-length 1748 :load-duration 2021/1000)
                                   (:system "babel-streams" :status :OK :log-blob-key "1098464" :log-byte-length 1814 :load-duration 121/40)
                                   (:system "babel-tests" :status :OK :log-blob-key "1093464" :log-byte-length 1811 :load-duration 2011/500)))))))

```


The follwing objects and properties extist:

**Database**
- `:schema` integer, the database schema version.
- `:runs` list of test run objects.

**Test run**
Represents test results of some libraries on a single lisp implementation.
- `:descr` Test run description object.
- `:results` list of library result objects.
  
**Test run description**
- `:lisp` Lisp implementation identifier - a string, for example `"clisp-2.49-unix-x86_64"`,
          `"cmu-20c_release-20c__20c_unicode_-linux-x86"`, `"sbcl-1.0.54-linux-x64"`
- `:lib-world` A string naming the set of libraries and their versions used during testing,
          for example `"quicklisp 2012-07-03"`, `"quicklisp 2012-08-11"`.
- `:time` start time of the test run, as returned by `(cl:get-universal-time)`,
          for example `3568041557`
- `:run-duration` Number of seconds taken by the test run, for example `10949`.
          Note, if during the test run the computer was hibernated, the hibernation
          time is included into this value.
- `:contact-email` String. Contact email of the person submitted the test results,
         for example `"avodonosov@yandex.ru"`. It is not guaranteed the email
         is valid. It may be whatever value user specified in the `test-grid-agent:user-email`.

**Library result**
- `:libname` Name of the library tested - a keyword, like `:babel`, `:alexandria`.
- `:status` Result of the library test suite. Present only if the library has testsuite adapter for cl-test-grid.
    Possible values: 
    - `:ok` None of the testcases has failed.

    - `:fail` Either some test cases failed but the test
              framework does not allow to distinguish
              particular test case, or some problem
              prevented the test suite from running at all.
              Example of such a problem may be that the
              testsuite or one of it's dependencies
              doesn't compile/load due to errors
              in lisp code; or absense of necessary
              foreign library on the test system.

    - `:no-resource` This status is designed to represent
        situations when testsuite can not be run due
        to absense of necessary enviromnent.

        For example, CFFI test suite needs a small
        C library to be build. On Windows user must
        do it manually. If this library is not found,
        testgrid adapter of the CFFI test suite returns `:no-resource`.
         
        Or, external-program test suite can only be
        run on *nix platforms. On Windows testgrid
        adapter returns :no-resource.

        The `:no-resource` handling in testsuite adapters
        is optional, as every testsuite may have different
        requirements.

        Today, most testsuite adapters in testgrid
        do not implemente such a handling, and
        in case of any problems when running
        the tests `:fail` is recorded.

    - `:crash` means the child lisp process running the test suite
        terminated without returning a result;

    - `:timeout`  means that the child lisp process
        hasn't finished in a specified timeout.

    - An extended test status object

- `:test-duration` Number of seconds taken by the library testsuite.
    Present only if the `:status` present.
    Often a fractional number, for example `3519/500`. Includes
    time taken by `ql:quickload` of the required systems.
- `:log-blob-key` String key under which the testsuite log is available online
    Present only if the `:status` present.
    at cl-test-grid.appspot.com/blob?key=<log-blob-key>. Example `"1qulq3d387"`.
- `:log-byte-length` Length of the testsuite log file, integer.
    Present only if the `:status` present.
    Note, the maximum length of the only copy of the file is 100 000 bytes,
    even if `:log-byte-length` value is greater. When longer files are submitted
    online, a section in the middle of the file is cut out.
- `:load-results` List of load result objects for all the ASDF systems of that library.

**Extended test status**
Returned when the testsuite adapter knows how to distinguish
separate testcases in the test framework used.
- `:failed-tests`  list of failed testcase names (strings)
- `:known-to-fail` list of testcases known to fail in this test framework.
    If the testcase specified here is absent in `:failed-tests` then
    this is an unexpected OK; if this testcase is present in `:failed-tests`
    than this is a known failure.

**Load result**
Represents result of `ql:quickload` for an ASDF system.
- `:system` Name of the ASDF system - a string, like `"arnesi"`, `"anaphora"`
- `:status` One of:
   - `:ok` means the load operation succeeded;

   - `:fail` means the load operation failed;

   - `:crash` means the child lisp process loading asdf
              system exited without returning a result;            

   - `:timeout` means that the child lisp process
                hasn't finished in a specified timeout time.

- `:load-duration` Number of seconds taken by the operation.
   Often a fractional number, for example `378/125`.
- `:log-blob-key` The same as described for library result objects
- `:log-byte-length` The same as described for library result objects


