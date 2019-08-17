;; Execute it form by form with C-x C-e in SLIME, as a tutorial.
(pushnew "directory/of/sptm-asdf/" asdf:*central-registry* :test #'equal)
(ql:quickload :sptm)

(defpackage :stpm-example (:use :cl))
(in-package :stpm-example)

;; This example operates on two bank accounts: A and B
;; The database is a plist mapping account names to account
;; objects, which are also plists, for simplicity.
(defun new-db ()
  '(:a (:balance 0) :b (:balance 0)))

;; Helper utility to write pure-functional
;; operations on plists. For bigger
;; projects a toolset of functional data
;; structures may be used, for example
;; http://common-lisp.net/project/fset/
(defun updated-plist (plist prop new-value)
  "Returns PLIST with the specified property
changed to NEW-VALUE."
  (let ((new (copy-list plist)))
    (setf (getf new prop) new-value)
    new))

(assert (= 2 (getf (updated-plist '(:a 1 :b 1) :a 2)
                   :a)))

;;; Database operations.
;;; Just lisp data modifications, persistence doesn't even mentioned.
;;; We only need to have DB as the first argument, and return
;;; new DB as a result, and functions must be ready that the other
;;; parameters may be deserialized from log.

(defun deposit (db account-name amount)
  (let* ((account (getf db account-name))
         (new-account (updated-plist account
                                     :balance
                                     (+ amount (getf account :balance)))))
    (updated-plist db account-name new-account)))

(deposit (new-db) :a 3)
;; => (:A (:BALANCE 3) :B (:BALANCE 0))

(defun withdraw (db account-name amount)
  (let ((account (getf db account-name)))
    (when (> amount (getf account :balance))
      (error "Insufficient funds are available in the account ~A to withdraw ~A"
             account-name amount))
    (deposit db account-name (- amount))))

(withdraw (deposit (new-db) :a 3) :a 1)
;; => (:A (:BALANCE 2) :B (:BALANCE 0))

(defun transfer (db from-account to-account amount)
  (deposit (withdraw db from-account amount)
           to-account
           amount))

(transfer (deposit (new-db) :a 10)
          :a :b 3)
;; => (:A (:BALANCE 7) :B (:BALANCE 3))

;;; Persistence for this database


(defparameter *sptm-demo-credentials*

  ;; we use functions sptm:credfuscate / sptm:decredfuscate to avoid
  ;; storing the credentials in the public repo
  (sptm:decredfuscate
   '("domda dae agni s ada daga fqttglah"
     "qbezbqb g oayb lbmbxbhyb rb oaybqxcf aqaqaof brfg lkambtadbq ibmbi ahcdayb ebq g"))

  "Credentials of an Amazon Web Services account having write access
to our demo Amazon S3 bucket and Amazon SimpleDB domain.
In the form (\"Access Key Id\" \"Secret Access Key\").")



(defun make-demo-transaction-log (name)
  "Creates a transaction log persisted to the demo storage."
  (make-instance 'sptm:aws-transaction-log
                 :name name
                 :s3-bucket "sptm-demo"
                 :simpledb-domain "sptmdemo"
                 :simpledb-endpoint-host "sdb.eu-west-1.amazonaws.com"
                 :credentials *sptm-demo-credentials*))

(defparameter *log* (make-demo-transaction-log "sptm-demo"))

;; Anyone can write to this log using the credentials
;; published above. Lets ensure we execute only
;; known functions when we read transactions from the log.
(defun transaction-allowed-p (fun-symbol)
  (member fun-symbol '(deposit withdraw transfer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now lets play with the database.

;;; Lower-level SPTM functions:

;; Retrieve whatever other people have left in the DB:
(defparameter *d*
  (sptm:roll-forward *log*
                     (make-instance 'sptm:versioned-data :version 0 :data (new-db))
                     'transaction-allowed-p))

;; Now *d* is a versioned-data instance holding the
;; latest data. Examine it:
(sptm:version *d*)
(sptm:data *d*)

;; Execute some transactions. You can do
;; it from different lisp processes and see how
;; data is synchronized.
(setf *d* (sptm:exec-transaction *log* *d*
                                 'deposit (list :a 7)
                                 'transaction-allowed-p))

(setf *d* (sptm:exec-transaction *log* *d*
                                 'transfer (list :a :b 3)
                                 'transaction-allowed-p))

;;; Replica is a "higher" level convenience class, which combines
;;; versioned-data, transaction log and a local snapshot file.
(defparameter *r*
  (make-instance 'sptm:replica
                 :transaction-log *log*
                 :transaction-checker 'transaction-allowed-p
                 :local-snapshot-file "sptm-demo-replica.lisp"
                 :vdata (make-instance 'sptm:versioned-data :version 0 :data (new-db))))

(sptm:sync *r*)
;; The above call reads local snapshot if exists.
;; Then, if online storage has any newer changes, retrieves
;; these changes and stores local snapshot of new data.

;; Examine the data in the replica
(sptm:data *r*)
(sptm:version *r*)

;; Execute some transaction using the replica,
;; saving the local snapshot if the transaction
;; executed successfully.
(sptm:repli-exec-save *r* 'withdraw (list :a 3))

;; Save online snapshot of the current data version
;; and remove all the transactions and snapshots
;; of older versions from the online storage.
(sptm:save-snapshot (sptm:transaction-log *r*)
                    (sptm:vdata *r*))
(sptm:delete-records (sptm:transaction-log *r*)
                     :below-version (sptm:version *r*))

;; Delete all the snapshots and transactions:
(sptm:delete-records (sptm:transaction-log *r*))
