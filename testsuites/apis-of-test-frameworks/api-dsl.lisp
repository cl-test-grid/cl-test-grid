(defpackage #:api-dsl
  (:use :cl)
  (:export #:proclfun))

(in-package #:api-dsl)

;; To specify an API of a common-lisp module it's enough to specify
;; set of symbols - a package.
;; 
;; Example:
;;
;; (defpackage #:some-api
;;   (:use :cl)
;;   (:export #:plus))
;;
;; This is sufficient to compile any code dependent on the
;; some-api:plus.
;;
;; But we want to provide more detailed documentation for 
;; the function (parameter types, return value, etc.)
;;
;; The PROCLFUN helper macro specifies that a particular 
;; symbol is a funtion, with particular argument types and 
;; return value, and sets a doc string for that function.
;;
;; Example of use:
;;
;;   (proclfun plus ((a number) (b number)) number
;;     "Returns the sum of two numbers.")
;;
;; The syntax similar to defgeneric:
;;
;;   (defgeneric plus ((a number) (b number))
;;     "Returns the sum of two numbers.")
;;
;; But proclfun also allows to specify the return type
;; (in our case it's number); and the function plus
;; is proclaimed to be a function, not a generic-function.
;;
;; The above proclfun macro call expands into these calls:
;;
;;   (PROCLAIM '(FTYPE (FUNCTION (NUMBER NUMBER) NUMBER) PLUS))
;;   (SETF (DOCUMENTATION 'PLUS 'FUNCTION) "Returns the sum of two numbers.")
;;
;; Thus proclfun helps to eleminate the "Undefined function PLUS"
;; compiler warnings when compiling client code.
;;
;; Of course, we could use defgeneric. But we just want
;; to demostrate (and explore) that it's possible to 
;; form an API implemented by usual functions, and still 
;; have client code independent on the function implementation 
;; presense or correctness (being compilable, and loadable).
;;
;; This macro hasn't been tested extensively yet. In particular 
;; the code for mixng &key with &rest or with &allow-other-keys
;; arguments and such is not tested.
(defmacro proclfun (name lambda-list return-type &body documentation)
  (let* ((key-seen-p nil)
         (arg-types (mapcar #'(lambda (arg-spec)
                                (cond ((eq arg-spec '&key) 
                                       (setf key-seen-p t) arg-spec)
                                      ((member arg-spec '(&optional &rest &allow-other-keys))
                                       arg-spec)
                                      (t (if key-seen-p 
                                             (etypecase (first arg-spec)
                                               ;; no default value: &key (:a integer) -> (:a integer)
                                               (symbol arg-spec)
                                               ;; with default value: &key ((:a integer) 0) -> (:a integer)
                                               (cons (first arg-spec)))
                                             (etypecase (first arg-spec)
                                               ;; no default value: &optional (a integer) -> integer
                                               (symbol (second arg-spec))
                                               ;; with default value: &optional((a integer) 0) -> integer
                                               (cons (second (first arg-spec))))))))
                            lambda-list)))
    `(progn
       ;; todo: use ,name only once
       (proclaim '(ftype (function ,arg-types ,return-type) ,name))
       (setf (documentation ',name 'function) ,@documentation)
       ',name)))

#|
More examples:

;; just a function with parameters
(proclfun plus ((a number) (b number)) number
  "Returns the sum of two numbers")

(defun plus (a b)
  (+ a b))

;; the same function, but with optional parameters
(proclfun plus (&optional (a number) (b number)) number
  "Returns the sum of two numbers")

(defun plus (&optional a b)
  (+ a b))

;; the same, but the optioanal parameter have default values
(proclfun plus (&optional ((a number) 0) ((b number) 0)) number
  "Returns the sum of two numbers")

(defun plus (&optional (a 0) (b 0))
  (+ a b))

;; now with key parameters
(proclfun plus (&key (:a number) (:b number)) number
  "Returns the sum of two numbers")

;; the same, but the key parameters have default values
(proclfun plus (&key ((:a number) 0) ((:b number) 0)) number
  "Returns the sum of two numbers")

(defun plus (&key (a 0) (b 0))
  (+ a b))

|#
