(defpackage #:meta-info-from-quicklisp
  (:use :cl)
  (:export #:extract-and-save-asdf-system-dependencies
           #:get-dependencies
           #:get-system->project))

(in-package #:meta-info-from-quicklisp)

#|
;;;; To extract the meta info from your
;;;; quicklisp and save it into an intermediate file
;;;; (needs to be done only only once, as we have the intermediate
;;;; file commited):

  (extract-and-save-meta-info-from-quicklisp "C:\\Users\\anton\\quicklisp\\dists\\quicklisp\\systems.txt")
  => NIL

;;;; To use the meta info from the intermediate file:

  (get-dependencies)
  => #<hash-table {system-name: (dependency-system-names...),
                   ...}> 

  (get-system->project)
  => #<hash-table {system-name: project-name,
                   system-name: project-name,
                   ...}> 
|#

(defparameter +persistence-file+ (test-grid-reporting::src-file "meta-info-quicklisp-2012-09-09.lisp")
  "This is where dependencies are save to and loaded from.")

(defun skip-header (stream file-pathname)
  (let ((header (read-line stream)))
    (unless (and (plusp (length header))
                 (char= (char header 0) #\#))
      (error "Bad header line in ~A -- ~S"
             file-pathname header))))

(defun extract-meta-info (quicklisp-index-file)
  "Returns an alist mapping each ASDF system name in Quicklisp
to a list of it's dependencies. QUICKLISP-INDEX-FILLE
is a pathname of a quicklisp/dists/quicklisp/systems.txt."
  (let (system->dependencies
        system->project)
    (with-open-file (stream quicklisp-index-file)
      (skip-header stream quicklisp-index-file)
    
      (loop
         for line = (read-line stream nil)
         for words = (and line (ql-util:split-spaces line))
         while line do
         (let ((project-name (first words))
               (system-name (third words))
               (dependencies (nthcdr 3 words)))
           (push (cons system-name dependencies)
                 system->dependencies)
           (push (cons system-name project-name)
                 system->project))))
    (assert (= (length (remove-duplicates system->dependencies :key #'first :test #'string=))
               (length system->dependencies)))
    (list :system->dependencies (sort system->dependencies #'string< :key #'first)
          :system->project (sort system->project #'string< :key #'first))))

(defun extract-and-save-meta-info-from-quicklisp (ql-index-file)
  (test-grid-utils::write-to-file (extract-meta-info ql-index-file)
                                  +persistence-file+)
  nil)

(defun get-dependencies ()
  "Returns a hash table from ASDF system name (string) to a list
if the system dependencies (list of system name strings)."
  (alexandria:alist-hash-table (getf (test-grid-utils::safe-read-file +persistence-file+)
                                     :system->dependencies)
                               :test #'equal))

(defun get-system->project ()
  "Returns a hash table from ASDF system name (string) to the project name (string)."
  (alexandria:alist-hash-table (getf (test-grid-utils::safe-read-file +persistence-file+)
                                     :system->project)
                               :test #'equal))