;;;; tests.lisp
;;;; Test suite for email-validator

(defpackage :email-validator.tests
  (:use :cl :email-validator)
  (:export #:run-tests))

(in-package :email-validator.tests)

(defparameter *test-count* 0)
(defparameter *pass-count* 0)
(defparameter *fail-count* 0)

(defun test (description expected-result actual-result)
  "Test helper function"
  (incf *test-count*)
  (if (eq expected-result actual-result)
      (progn
        (incf *pass-count*)
        (format t "✓ ~A~%" description))
      (progn
        (incf *fail-count*)
        (format t "✗ ~A (expected ~A, got ~A)~%" description expected-result actual-result))))

(defun run-tests ()
  "Run all tests"
  (setf *test-count* 0)
  (setf *pass-count* 0)
  (setf *fail-count* 0)
  
  (format t "~%=== Running Email Validator Tests ===~%~%")
  
  ;; Basic structure tests
  (format t "Basic Structure Tests:~%")
  (test "Simple valid email" t (email-p "user@example.com"))
  (test "No @ sign" nil (email-p "userexample.com"))
  (test "Multiple @ signs" nil (email-p "user@@example.com"))
  (test "@ at start" nil (email-p "@example.com"))
  (test "@ at end" nil (email-p "user@"))
  (test "Empty string" nil (email-p ""))
  (test "Not a string" nil (email-p 123))
  
  ;; Local part tests
  (format t "~%Local Part Tests:~%")
  (test "Email with dots" t (email-p "firstname.lastname@example.com"))
  (test "Email with plus sign" t (email-p "user+tag@example.com"))
  (test "Email with underscore" t (email-p "user_name@example.com"))
  (test "Email with hyphen" t (email-p "user-name@example.com"))
  (test "Special chars" t (email-p "!#$%&'*+-/=?^_`{|}~@example.com"))
  (test "Leading dot local" nil (email-p ".user@example.com"))
  (test "Trailing dot local" nil (email-p "user.@example.com"))
  (test "Consecutive dots" nil (email-p "user..name@example.com"))
  (test "Space in local" nil (email-p "user name@example.com"))
  (test "Invalid chars <>" nil (email-p "user<>@example.com"))
  (test "Local too long (65)" nil (email-p 
                                    (concatenate 'string 
                                                (make-string 65 :initial-element #\a)
                                                "@example.com")))
  (test "Max length local (64)" t (email-p 
                                   (concatenate 'string 
                                               (make-string 64 :initial-element #\a)
                                               "@example.com")))
  
  ;; Domain tests
  (format t "~%Domain Tests:~%")
  (test "Subdomain" t (email-p "user@mail.example.com"))
  (test "Multiple subdomains" t (email-p "user@mail.server.example.com"))
  (test "Numbers in domain" t (email-p "user@example123.com"))
  (test "All numeric domain" t (email-p "user@123.456.789.012"))
  (test "Domain with hyphen" t (email-p "user@example-domain.com"))
  (test "IPv4 literal" t (email-p "user@[192.168.1.1]"))
  (test "IPv6 literal" t (email-p "user@[IPv6:2001:db8::1]"))
  (test "Single label domain" nil (email-p "user@example"))
  (test "Leading hyphen domain" nil (email-p "user@-example.com"))
  (test "Trailing hyphen domain" nil (email-p "user@example-.com"))
  (test "Space in domain" nil (email-p "user@exam ple.com"))
  (test "Empty label" nil (email-p "user@example..com"))
  (test "Leading dot domain" nil (email-p "user@.example.com"))
  (test "Trailing dot domain" nil (email-p "user@example.com."))
  (test "Invalid IPv4" nil (email-p "user@[256.1.1.1]"))
  (test "Incomplete IPv4" nil (email-p "user@[192.168.1]"))
  (test "Label too long (64)" nil (email-p 
                                    (concatenate 'string 
                                                "user@"
                                                (make-string 64 :initial-element #\a)
                                                ".com")))
  (test "Max length label (63)" t (email-p 
                                    (concatenate 'string 
                                                "user@"
                                                (make-string 63 :initial-element #\a)
                                                ".com")))
  
  ;; Edge cases
  (format t "~%Edge Cases:~%")
  (test "Single char local" t (email-p "a@example.com"))
  (test "Single char labels" t (email-p "a@b.c"))
  (test "Mixed case" t (email-p "User@Example.COM"))
  (test "Unicode in local" nil (email-p "üser@example.com"))
  (test "Unicode in domain" nil (email-p "user@exämple.com"))
  (test "Email too long (321)" nil (email-p 
                                     (concatenate 'string 
                                                 (make-string 64 :initial-element #\a)
                                                 "@"
                                                 (make-string 256 :initial-element #\b)
                                                 ".com")))
  
  
  ;; Summary
  (format t "~%=== Test Summary ===~%")
  (format t "Total tests: ~A~%" *test-count*)
  (format t "Passed: ~A~%" *pass-count*)
  (format t "Failed: ~A~%" *fail-count*)
  (format t "Success rate: ~,1F%~%~%" (* 100 (/ *pass-count* *test-count*)))
  
  (if (= *fail-count* 0)
      (progn
        (format t "All tests passed! ✓~%")
        t)
      (progn
        (format t "Some tests failed. Please review.~%")
        nil)))