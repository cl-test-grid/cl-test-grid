;; find all ASDF systems in the current Quicklisp
;; distro, which have "test" in the system name.
;;
;; Print the systems sorted by their download count,
;; as specified in the quicklisp download statistics
;; file.

(ql:quickload "cl-ppcre")

(defun parse-download-statistics (filename)
  "Returns a hash table, mapping library name to it's download
count."
  (let ((statistics (make-hash-table :test 'equal)))
    (with-open-file (in filename)
      (let ((regex (cl-ppcre:create-scanner "([\\d]+) ([^\\s]*)")))
        (do ((line (read-line in nil) (read-line in nil)))
            ((not line))
          (cl-ppcre:register-groups-bind ((#'parse-integer download-count) libname)
              (regex line :sharedp t)
            (setf (gethash libname statistics) download-count)))))
    statistics))


(defparameter *download-statistics*
  (parse-download-statistics "quicklisp-download-statistics-2012.txt")
  "A hashmap from project name to its download count from quicklisp.")

(ql-dist:provided-systems (ql-dist:release "iterate"))

(defun project-by-system (sysname)
  (ql-dist:name (ql-dist:release (ql-dist:system sysname))))


;; all the systems in the current quicklisp distor
;; version, having the word "test" somewhere
;; inside the system name

(defun detect-test-system (lib)
  (handler-case
      (progn
        (ql:quickload lib)
        (list :lib lib :can-quickload t))
    (serious-condition (e)
      (format t "can't load: ~A: ~A~%" lib e)
      (list :lib lib :can-quickload nil))))

(defun system-download-count (sysname)
  (let ((projname (project-by-system sysname)))
    (gethash projname *download-statistics*)))

(defparameter *test-systems*
  (sort (remove-if-not (alexandria:curry #'search "test")
                       (mapcar #'ql-dist:name (ql-dist:provided-systems (ql-dist:dist "quicklisp"))))
        #'(lambda (sysname-a sysname-b)
            (> (or (system-download-count sysname-a) 0)
               (or (system-download-count sysname-b) 0)))))

(dolist (sysname *test-systems*)
  (format t "~A ~A (~A)~%" (system-download-count sysname) sysname (project-by-system sysname)))


