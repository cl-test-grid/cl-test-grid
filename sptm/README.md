# SPTM - Shared Persistent Transactional Memory

Simple storage for lisp data, allowing distributed
participants (processes) to interact via shared
in-memory data structure.

Based on ideas of in-memory persistence and optimistic concurrency
control. Operates via transaction log stored on Amazon Web Services
(S3 and SimpleDB).

## Informal Description of the Ideas
### In-Memory Persistence

This idea is also known as Prevalence (there is also Common Lisp
library [cl-prevalence](http://common-lisp.net/project/cl-prevalence/)).

All persistent data modifications are expressed as named functions
with parameters. For example
   
``` common-lisp
   (add-user database :id 1 :name "anton")
   (add-order database :id 117 :user 1 :book "AIMA" :count 1)
   (delete-user database :id 1)
```

Here `database` is some datastructure where you store the information,
and `add-user`, `add-order`, `delete-user` are your functions which
modify the database.
   
To make the modifications persistent they are executed in transactions
``` common-lisp
   (execute-transaction database 'add-user (list :id 1 :name "anton"))
   (execute-transaction database 'add-order (list :id 117 :user 1 :book "AIMA" :count 1))
   (execute-transaction database 'delete-user (list :id 1))
```
   
The `execute-transaction` first applies the specified function to arguments,
thus modifying the database, and then records the function name and all the arguments,
except for the `database` argument, to a transaction log. In cl-prevalence
transaction log is a file on local file system.

When application process is restarted after a crash or normal termination,
it reads the transaction log and executes the functions specified there.
In result all the database modifications are repeated, so the
application sees its in-memory database persistent across restarts.

Besides the transaction log, a complete snapshot of database can be stored.
Therefore the full procedure of restoring the in-memory database state
when application is restarted is to read the snapshot if present, and then
execute the transactions recorded in the log after the snapshot was made.

To make this working, all these functions have `database` as their first
parameter, and all other parameters must be serializable.

The functions must be ready to receive arguments deserialized from log/snapshot,
therefore we typically want to pass some string/numeric IDs as arguments,
not whole object references. For example `add-order` accepts user ID as
the parameter, but not a `user` object. Otherwise we might end up
dealing with many copies of the same `user` object, duplicated by
deserialization.

In contrast to cl-prevalence, in SPTM the transaction log and snapshots
are stored online and may be used by several processes to share data.

For example cl-test-grid agents on different machines collect test
results and publish them by recording `add-test-run` transaction
to the online log. Then cl-test-grid admin executes all these transactions
on his machine and so receives the data collected.
     
### Concurrency Control

In cl-prevalence transactions in different application threads
may be serialized using global lock aquired by the `execute-transaction` function,
effectively serializing the transactions (which remains fast, as appending
to a file is a very quick operation).

But in SPTM the transactions are executed in different processes
running on different machines over Internet. How to synchronize them?

Here we use ideas from optimistic concurrency approach in Clojure's
software transactional memory.

It has similarities with Prevalence - the data modifications are expressed as
functions with parameters. But the functions are pure: they
compute new version of data, instead of modifying the data destructively;
also the functions do not perform any IO.

After the function has computed new version of data, an attempt to
commit the transaction is performed (in Clojure this means storing
new data value in a Ref; in SPTM this means adding a transaction record
to the transaction log).

If during the commit attempt it is determined that another transaction(s)
has commited since our transaction has started, the value computed by
the function is discarded, the data is updated to reflect the changes
made by concurrent (remote in case of SPTM) transactions,
and our transaction is restarted again - execute the function and try
to commit.

This protocol guarantees that everyone replaying the transaction log
executes the functions on exactly the same data as the process
which recorded this transaction to the log.

Data consistency checks are implemented in transaction functions.
For example, `withdraw-from-bank-account` function should check
that the account has enough funds, and signal a condition otherwise.
If a concurrent transaction has spent all the funds, our transation
signlas the condition, and user receives notification. Thus only
consistent modifications of database are comitted to the transaction log.

Note also, that not all transactions require synchronization.
For example `add-test-run` transaction in cl-test-grid: we just
need to add test run results, this operation is not tied by
any consistency constraint with other data in the database,
so we can just freely record it. The `sptm:record-transaction`
function provides this possibility.

## Security

So, we retrieve function names from transaction log and call these functions.

If the log is writable by untrusted parties, we can protect ourselves with
a `transaction-checker` - a predicate on the function name. SPTM API
accepts such a predicate as a parameter and uses it to verify that
transactions refer only allowed functions before executing the transactions.

## Example

Please see [example.lisp](example.lisp).
   
## API and the Source Code

- [versioned-data.lisp](versioned-data.lisp) - start here. Defines a two-slot class
  `versioned-data` which can annotate arbitrary data with a `fixnum` version.
  Also defines a protocol (set of generic functions) for transaction log.
  Then, based on the transaction log protocol defines the main functions to
  operate on `versioned-data` via transaction log: `roll-forward`, 
  `execute-transaction`, `record-transaction`.
- [aws-transaction-log.lisp](aws-transaction-log.lisp) - implements the transaction
  log protocol using Amazon Web Services: S3 and SimpleDB.
- [replica.lisp](replica.lisp) - convenience class `replica`, combines transaction log,
  versioned-data and a local snapshot of the versioned-data, stored in a file.
- [amazon-simple-db.lisp](amazon-simple-db.lisp) - private tools to work
  with Amazon SimpleDB.
- [package.lisp](package.lisp) - lists all the public API functions.

## Storage Space at Amazon

You may use the demonstrational S3 bucket and SimpleDB domain as shown
in the [example.lisp](example.lisp). Many independent transaction logs may be stored
on the same storage - just give every transaction log different name.
The function `sptm-example::make-demo-transaction-log` demonstrates how to do this.

Please don't store too many data on this storage. Also the storage
owner reserves right to delete the data at any time.

If you want to use your own storage, here are the steps:
- Create an S3 bucket. This may be done via Amazon WS console.
- Create a SimpleDB domain. Note, Amazon doesn't like dashes in S3
  domain names, so use "yourdomain", but  not "your-domain". The following
  call may be used to create a domain on desired SimpleDB endpoint host:
``` common-lisp
       (sptm::create-simpledb-domain "yourdomain"
           '(:credentials ("YOUR-ACCESS-KEY-ID" "YOUR-SECRET-ACCESS-KEY") :host "sdb.eu-west-1.amazonaws.com"))
```
- [Optional] If you provide access to the log to 3rd parties, you
  may want to provide this access via separate user account created in
  Amazon Identity Manager (so called IAM users). That way you may quickly
  revoke access, and limit the access by these bucket and domain only.
  
  This separate user account must be granted the following permissions:
  - S3 bucket: PutObject, GetObject, DeleteObject;
  - SimpleDB domain: PutAttributes, GetAttributes, DeleteAttributes, Select.
  
  The resulting security policy for this user will look similar to this:

``` json
     {
       "Statement": [
         {
           "Sid": "Stmt1355322322649",
           "Action": [
             "s3:DeleteObject",
             "s3:GetObject",
             "s3:PutObject"
           ],
           "Effect": "Allow",
           "Resource": [
             "arn:aws:s3:::sptm-demo/*"
           ]
         },
         {
           "Sid": "Stmt1355322452686",
           "Action": [
             "sdb:DeleteAttributes",
             "sdb:GetAttributes",
             "sdb:PutAttributes",
             "sdb:Select"
           ],
           "Effect": "Allow",
           "Resource": [
             "arn:aws:sdb:eu-west-1:321537799465:domain/sptmdemo"
           ]
         }
       ]
     }
```
     
## Further Notes and Restrictions
### System Time on Your Machine

Amazon Web Services require each request to be annotated with
a timestamp. If the timestamp is different by more than 15 minutes
form system time of Amazon server, the request is rejected - 
http://docs.amazonwebservices.com/AmazonS3/latest/dev/RESTAuthentication.html#RESTAuthenticationTimeStamp.
Therefore your machine must have correct system time to use
SPTM.

### Initial DB Content
    
If database is some kind of collection or map where
the data is stored by transactions, the first question is
how to create the empty database (empty map or collection).

Possible approach is to have the very first transaction
to be a function returning this fresh database.

Another option is to write your application so, that
`versioned-data` with version 0, which is created before
executing any transactions, is always created with
`data` slot initialized to a fresh database. All the
further transactions can expect the database to be
initialized. This little trick is used in [example.lisp](example.lisp).
    
### The Default Serialization is via `cl:write` / `cl:read`.

The default serialization of the transactions and snapshots stored
online, and of the replica local snapshots is via `cl:write` / `cl:read`
(with `cl:*read-eval*` bound to `nil` of course). To use different
serialization it is necessary to customize the code, by overriding
methods of certain generic functions.

### Clojure Has Many Refs, SPTM Operates on a Single Big Database.

Using many independent Ref objects allows Clojure to reduce
interference between transactions - transactions operating
on different Refs are not conflicting and no retries are necessary
for them, they are just commited freely.

For cl-test-grid the SPTM approach of a single database is enough.
