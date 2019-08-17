;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

;;;; This module implements obfuscation of base64 striing so it doesn't look
;;;; base64. The motivation is to avoid AWS credentials search by pattern
;;;; (a regex, for example) if you need to publish them, but don't 
;;;; want to draw attention to that.
;;;;
;;;; The obfuscated form looks like a sentence - space separated "words",
;;;; i.e. short sequences of lower case latin caracters.
;;;;
;;;; The 23 latin charactes form a kind of base23 encoding.
;;;; But in our implementation it is a variable length encoding -
;;;; some base64 characters translate to 1 output character,
;;;; some to 2 characters.
;;;;
;;;; Each base64 character is first mapped to its index in the
;;;; base64 alphabet (from 0 to 63). Then this index is printed
;;;; in our variable length base23 encoding.
;;;;
;;;; Which is:
;;;; - only needs to represent numbers between 0 and 63
;;;; - the first 3 latin characters are used for the optional
;;;;   most significant position in the representation:
;;;;   a = 20, b = 40, c = 60
;;;; - the remaining 20 latin caracters are used for the
;;;;   least significant position, and represent numvers
;;;;   from 0 to 19.
;;;;
;;;; So base64 character `F` is at the position 5 (zero based)
;;;; in the base64 alphabet, it is represented by the
;;;; character `i` which has position 5 between the last 20 latin
;;;; characters.
;;;;
;;;; Base64 character 'm' is represented as `ay' where 'a' stands
;;;; for 20 and 'y' stands for 18.
;;;;
;;;; At the end, spaces inserted into encoded string, at random
;;;; places, in avearate after every 5 characters.

(defpackage #:obfuscate-base64
  (:use :cl)
  (:export #:obfuscate-base64-string
           #:deobfuscate-base64-string))

(in-package #:obfuscate-base64)

(defun char-indexes (str)
  (let ((hash (make-hash-table :test 'eql)))
    (dotimes (i (length str))
      (setf (gethash (aref str i) hash)
            i))
    hash))

(defparameter +base64-digits+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(defparameter +base64-index-by-digit+ (char-indexes +base64-digits+))

(defparameter +high-digits+ "abc")
(defparameter +high-index-by-digit+ (char-indexes +high-digits+))

(defparameter +low-digits+ "defghiklmnopqrstvxyz")
(defparameter +low-index-by-digit+ (char-indexes +low-digits+))

(defun encode-base64-digit (base64-digit stream)
  (let ((index (gethash base64-digit +base64-index-by-digit+)))
    (assert index nil "~A is not a base64 digit")
    (multiple-value-bind (quot rem)
        (floor index (length +low-digits+))
      (when (> quot 0)
        (assert (<= quot (length +high-digits+))
                nil
                "~A" quot)
        (write-char (aref +high-digits+
                          ;; 1- because we don't encode zero in high position,
                          ;; so the first hight digit is not zero
                          (1- quot))
                    stream))
      (assert (< rem (length +low-digits+)))
      (write-char (aref +low-digits+ rem) stream))))

(defun decode-base64-digit (stream)
  (let* ((char (read-char stream))
         (high-index (gethash char +high-index-by-digit+)))
    (aref +base64-digits+
          (if high-index
              (+ (* (1+ high-index) (length +low-digits+))
                 (gethash (read-char stream)
                          +low-index-by-digit+))
              (gethash char +low-index-by-digit+)))))

;; test for all base64 digits
(dotimes (i (length +base64-digits+))
  (let ((digit (aref +base64-digits+ i)))
    (with-input-from-string (in (with-output-to-string (s)
                                  (encode-base64-digit digit s)))
      (assert (eql digit (decode-base64-digit in))))))

(defun empty-stream-p (stream)
  (eql :eof (peek-char nil stream nil :eof)))

(defun dostream-impl (stream body)
  (do () ((empty-stream-p stream))
    (funcall body (read-char stream))))

(defmacro dostream ((stream var) &body body)
  `(dostream-impl ,stream (lambda (,var) ,@body)))

(defun encode-base64-string (str)
  (with-output-to-string (out)
    (with-input-from-string (in str)
      (dostream (in char)
        (encode-base64-digit char out))))))

(defun decode-base64-string (str)
  (with-output-to-string (out)
    (with-input-from-string (in str)      
      (do () ((empty-stream-p in))
        (write-char (decode-base64-digit in) out)))))

(assert (string-equal +base64-digits+
                      (decode-base64-string
                       (encode-base64-string +base64-digits+))))

(defun obfuscate-base64-string (base64-string &optional (random-state *random-state*))
  "An utility which transforms a base64 string into a string looking like
a sentence - a number space separated sequences of lower latin caracters.
RANDOM-STATE is used to place spaces randomily in this representation."
  (let ((encoded (encode-base64-string base64-string)))
    (with-output-to-string (out)      
      (with-input-from-string (in encoded)
        (do () ((empty-stream-p in))
          (write-char (read-char in) out)
          (unless (empty-stream-p in)
            (when (= 0 (random 5 random-state))
              (write-char #\Space out))))))))

(defun deobfuscate-base64-string (obfuscated-string)
  "Decodes a string produced by OBFUSCATE-BASE64-STRING back to its original base64 form."
  (let ((without-spaces (with-output-to-string (out)      
                          (with-input-from-string (in obfuscated-string)
                            (dostream (in char)
                              (when (not (eql #\Space char))
                                (write-char char out)))))))
    (decode-base64-string without-spaces)))

(assert (string-equal +base64-digits+
                      (deobfuscate-base64
                       (obfuscate-base64 +base64-digits+))))
