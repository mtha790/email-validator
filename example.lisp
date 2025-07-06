;;;; example.lisp
;;;; Example usage of the email-validator library

;; Load the system
(asdf:load-system :email-validator)

(defun demo-email-validator ()
  "Demonstrate the email-validator functionality"
  (format t "~%=== Email Validator Demo ===~%~%")

  ;; Valid emails
  (format t "✓ Valid emails:~%")
  (dolist (email '("user@example.com"
                   "firstname.lastname@example.com" 
                   "user+tag@example.com"
                   "user_name@example.org"
                   "user-123@mail.example.com"
                   "test@[192.168.1.1]"
                   "test@[IPv6:2001:db8::1]"
                   "!#$%&'*+-/=?^_`{|}~@example.com"))
    (assert (email-validator:email-p email))
    (format t "  ~A~%" email))

  ;; Invalid emails  
  (format t "~%✗ Invalid emails:~%")
  (dolist (email '("plainaddress"
                   "@example.com"
                   "user@"
                   "user..name@example.com"
                   "user@example"
                   "user name@example.com"
                   "user@exam ple.com"
                   "user@-invalid.com"
                   "üser@example.com"))
    (assert (not (email-validator:email-p email)))
    (format t "  ~A~%" email))

  ;; Performance test
  (format t "~%Performance test (10,000 validations):~%")
  (let ((start-time (get-internal-real-time))
        (test-email "user@example.com"))
    (dotimes (i 10000)
      (email-validator:email-p test-email))
    (let ((end-time (get-internal-real-time)))
      (format t "  Time: ~,3F seconds~%" 
              (/ (- end-time start-time) internal-time-units-per-second))))

  (format t "~%Demo completed successfully!~%"))

(defun interactive-validator ()
  "Interactive email validation"
  (format t "~%=== Interactive Email Validator ===~%")
  (format t "Enter email addresses to validate (or 'quit' to exit):~%~%")
  
  (loop
    (format t "> ")
    (force-output)
    (let ((input (string-trim '(#\Space #\Tab #\Newline) (read-line))))
      (cond 
        ((string= input "quit") (return))
        ((string= input "") (format t "Please enter an email address.~%"))
        (t (format t "~A is ~:[❌ invalid~;✅ valid~]~%~%" 
                   input (email-validator:email-p input))))))
  
  (format t "Goodbye!~%"))

;; Run demo by default
(demo-email-validator)

;; Uncomment to run interactive mode
;; (interactive-validator)