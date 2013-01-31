Running Tests
=============

`test-grid-agent:agent` is a lisp object able
to manage test exectuion by subordinate lisp
implementations (executables) and submit test
resutls to server.

It is created with function `test-grid-agent:make-agent`
and has 3 required configuration properties:

- `lisps` - Paths to the lisp implementations
            that should be used to run tests.

- `preferred-lisp` - The lisp implementation used when
               it is necessary to perform an auxiliary task
               requiring a separte lisp process, for example
               downloading libraries to be tested.
               It is therefore desirable to specify here 
               a lisp implementation known to work reliable
               on your platform.

- `user-email` - Your email so that we know who is contributing
               the test results and can contact you. The
               email is also published in the test results
               reports so that library authors or other interested
               parties can contact you with questions about your platform.
               If you are strongly opposed to publish your email,
               you can specify just some nickname here.

Function `test-grid-agent:main` runs the agent.

It is necessary to perform `git pull` on agent sources
often.

There are template scripts demonstrating how to
load, cofigure and run agent by a single commant.

Please use [CCL](http://ccl.clozure.com/) - it is the development platform and the only
lisp known to run agent successfully. The template scripts
assume [Quicklisp](http://www.quicklisp.org/beta/) is installed and added to the CCL init file.

So, the steps:

1. `git clone git://github.com/cl-test-grid/cl-test-grid.git`
2. `cd cl-test-grid`
3. `cp run-agent.sh.sample run-agent.sh; chmod +x run-agent.sh`
4. `cp run-agent.sample.lisp run-agent.lisp`
5. Edit the _run-agent.sh_ (edit one line - the path to CCL).
6. Edit the _run-agent.lisp_ (paths to the lisp implementations, your email)
7. `./run-agent.sh`

Next time all you need is to just invoke `./run-agent.sh`. It will update the 
`cl-test-grid` from git, run tests and upload the results.

Agent keeps log files in the _cl-test-grid/work-dir/agent/logs/_,
where you can control what it has done.

Example crontab record to run agent at 10 o'clock every day:   
``` shell
  # minute hour day_of_month month day_of_week command
  0 10 * * * cd /home/testgrid/cl-test-grid/ && ./run-agent.sh
```

Details of what agent actually does
-----------------------------------

Simplified, the agent mode of operation may be represened
by the following pseudo code:

``` common-lisp
   (let ((current-quicklisp (update-quicklisp)))
     (loop for lisp in my-lisp-implementations
        (when (not (tested-already lisp current-quicklisp))
           (let ((results-dir (complete-test-run lisp (or (find-unfinished-test-run lisp current-quicklisp)
                                                          (make-new-test-run lisp current-quicklisp)))))
              (submit-results results-dir)
              (remember-tested lisp current-quicklisp)
              (cl-fad:delete-directory-and-files results-dir)))))
```

As you can see, the agent submits test results after
completing full test set on a single lisp implementation. 

The code, including the internal implementaton
of `complete-test-run` is organized so that agent can
be interrupted (computer rebooted or hibernated,
agent process killed). When started again, it continues
the work from the point of interruption.

Testing single lisp implementation may take from 1-2
hours up to 10 hours or more (for ABCL - ABCL has long
startup time, which becomes significant in our use case
as we run every test suite or ASDF system compilation
in a fresh lisp process).

**Caveat of killing the agent:** if you killed the agent process
(without rebooting the machine), the subordinate process
running current testsute or compiling current ASDF system
remains alive. Typically it takes less than a minute for
it to finish, but sometimes it may take longer (the
testsuite or library compilation may require longer
time; or, in the worst case, test suite may hang).
If you start agent again, it spawns new test running
process, which can interfere with the old one via file
system (.fasl files, output logs). Therefore it's better
to give the old child process time to finish before
starting the agent again.

Parallel execution of multiple agents
-------------------------------------

Agent operates sequentially.

During its work, agent keeps it's working data in
a directory specified by the cofiguration property
- `work-dir` - Defaults to the _&lt;cl-test-grid source code root&gt;/work-dir/agent/_

The agent takes measures to ensure there is only
one agent instance using this working directory.

This is acheaved by using a TCP port as an inter-process
lock. When started, agent tries to open a socket on
the port. If it is successful, the agent continues.
If the port is busy, the agent deduces there is
another agent instance running, logs a warning
and exists.

The port number is specified by the configuration
property
- `singleton-lock-port` defaults to 7685.

If you want to run several agent processes
and distirbute testing work between them,
you can assign each agent different set 
of lisp implemenations and give each
agent different working directory and lock
port.

Getting assistance
------------------

Feel free to contact us if you have any questions or
difficulties (see the mailing list address below).

Source Code Guide
=================

The key components, from bottom up:
- [`lisp-exe`](lisp-exe.lisp) Represents lisp implementation executable and allows to start
  child lisp process to execute some lisp code. Supports timeouts: if the child
  lisp process exceeds the specified timeout, it is killed and corresponding
  condition is signalled.
- On top of it the function [`complete-test-run`](complete-test-run.lisp) is built.
  It runs tests on the specified `lisp-exe` and quicklisp installation,
  and stores test results in a directory. If the testging process is
  interrupted and started again with the same output directory, it continues
  from the point of interruption.
- [`submit-test-run-results`](submit-test-run-results.lisp) submits log files and test
  results from test run output directory to online storage.
- Finally, on top of these components [`agent`](agent.lisp) object is build. It maintains
  private quicklisp installation in working directory and updates it to the
  latest version every time when started. It also remembers what lisp implenetations
  were tested on what quicklisp version. This information kept in 
  [`persistence`](persistence.lisp) - a simple plist saved in file.
  Also agent checks the configuration parameters provided by user and ensures only
  one agent instance is run at time, using a TCP port, as described above.
  The agent public API is specified in the [_package.lisp_].

The source files naming convention _proc-*.lisp_ is used for files executed
by child lisp processes started via `lisp-exe`.


  