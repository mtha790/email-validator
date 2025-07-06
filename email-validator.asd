;;;; email-validator.asd
;;;; ASDF system definition for email-validator

(defsystem "email-validator"
  :description "RFC-compliant email validation library for Common Lisp"
  :author "mtha790"
  :license "MIT"
  :version "1.0.0"
  :homepage "https://github.com/mtha790/email-validator"
  :bug-tracker "https://github.com/mtha790/email-validator/issues"
  :source-control (:git "https://github.com/mtha790/email-validator.git")
  :serial t
  :components ((:file "package")
               (:file "email-validator"))
  :in-order-to ((test-op (test-op "email-validator/tests"))))

(defsystem "email-validator/tests"
  :description "Test suite for email-validator"
  :depends-on ("email-validator")
  :serial t
  :components ((:file "tests"))
  :perform (test-op (op c) 
             (let ((result (symbol-call :email-validator.tests :run-tests)))
               (unless result
                 (error "Tests failed")))))