;;;; package.lisp
;;;; Package definition for email-validator

(defpackage :email-validator
  (:use :cl)
  (:export #:email-p)
  (:documentation "RFC-compliant email validation library for Common Lisp"))