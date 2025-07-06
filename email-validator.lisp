;;;; email-validator.lisp
;;;; RFC 5321/5322 compliant email address validation

(in-package :email-validator)

(defun email-p (email)
  "Returns T if EMAIL is a syntactically valid email address per RFC 5321/5322.
   
   Validates against the addr-spec production from RFC 5322, supporting:
   - Standard local-part@domain format
   - Special characters in local part as per RFC 5321
   - Domain names following DNS rules  
   - IP literals for both IPv4 and IPv6
   - Length constraints from RFC 5321 (64/253/320 character limits)
   
   Does NOT support (by design for security/simplicity):
   - Quoted strings or comments (RFC 5322 obs-* productions)
   - Internationalized domain names
   - Unicode characters outside ASCII"
  (when (stringp email)
    (let ((at-symbol-position (position #\@ email)))
      (and at-symbol-position
           (has-single-at-symbol-p email at-symbol-position)
           (within-rfc-length-limit-p email)
           (has-non-empty-parts-p email at-symbol-position)
           (rfc-compliant-local-part-p (subseq email 0 at-symbol-position))
           (rfc-compliant-domain-p (subseq email (1+ at-symbol-position)))))))

(defun has-single-at-symbol-p (email at-position)
  "Ensures email contains exactly one @ symbol (RFC 5321 requirement)."
  (not (position #\@ email :start (1+ at-position))))

(defun within-rfc-length-limit-p (email)
  "Enforces RFC 5321 maximum path length of 320 characters."
  (<= (length email) 320))

(defun has-non-empty-parts-p (email at-position)
  "Ensures both local and domain parts are non-empty."
  (and (> at-position 0)
       (< at-position (1- (length email)))))

(defun rfc-compliant-local-part-p (local-part)
  "Validates local part against RFC 5321 atext production rules.
   
   Rejects quoted strings and comments for security - these are often
   sources of parser confusion and potential bypass attempts."
  (and (within-local-part-length-limit-p local-part)
       (proper-dot-usage-p local-part)
       (every #'atext-character-p local-part)))

(defun within-local-part-length-limit-p (local-part)
  "RFC 5321 local-part must not exceed 64 octets."
  (and (> (length local-part) 0)
       (<= (length local-part) 64)))

(defun proper-dot-usage-p (local-part)
  "Dots cannot appear at start/end or consecutively (RFC 5321 dot-atom rules)."
  (and (not (char= (char local-part 0) #\.))
       (not (char= (char local-part (1- (length local-part))) #\.))
       (not (search ".." local-part))))

(defun atext-character-p (char)
  "Tests if character is valid in atext production (RFC 5322).
   
   Restricted to ASCII for security - Unicode can cause normalization
   issues and encoding attacks."
  (and (< (char-code char) 128)
       (or (alphanumericp char)
           (member char '(#\. #\! #\# #\$ #\% #\& #\' #\* #\+
                         #\- #\/ #\= #\? #\^ #\_ #\` #\{ #\| #\} #\~)))))

(defun rfc-compliant-domain-p (domain)
  "Validates domain part per RFC 5321 domain production.
   
   Supports both domain names and IP literals. Domain names must follow
   DNS label rules for interoperability."
  (if (ip-literal-format-p domain)
      (valid-ip-literal-p domain)
      (valid-dns-domain-p domain)))

(defun ip-literal-format-p (domain)
  "Checks if domain uses IP literal format [address]."
  (and (> (length domain) 0)
       (char= (char domain 0) #\[)))

(defun valid-dns-domain-p (domain)
  "Validates domain name following DNS label rules (RFC 1035).
   
   Requires at least two labels for practical email routing,
   though RFC technically allows single labels."
  (and (within-domain-length-limit-p domain)
       (proper-domain-structure-p domain)
       (let ((labels (domain-labels domain)))
         (and (minimum-label-count-p labels)
              (every #'valid-dns-label-p labels)))))

(defun within-domain-length-limit-p (domain)
  "RFC 1035 limits domain names to 253 characters."
  (and (> (length domain) 0)
       (<= (length domain) 253)))

(defun proper-domain-structure-p (domain)
  "Domains cannot start or end with dots (malformed)."
  (and (not (char= (char domain 0) #\.))
       (not (char= (char domain (1- (length domain))) #\.))))

(defun minimum-label-count-p (labels)
  "Requires at least two labels for practical email delivery."
  (> (length labels) 1))

(defun valid-dns-label-p (label)
  "Validates DNS label per RFC 1035 rules.
   
   Labels must start/end with alphanumeric for compatibility
   with all DNS implementations."
  (and (within-label-length-limit-p label)
       (alphanumeric-boundaries-p label)
       (every #'valid-hostname-character-p label)))

(defun within-label-length-limit-p (label)
  "DNS labels cannot exceed 63 octets (RFC 1035)."
  (and (> (length label) 0)
       (<= (length label) 63)))

(defun alphanumeric-boundaries-p (label)
  "DNS labels must start and end with alphanumeric characters."
  (and (alphanumericp (char label 0))
       (alphanumericp (char label (1- (length label))))))

(defun valid-hostname-character-p (char)
  "Valid hostname characters: letters, digits, hyphens (ASCII only)."
  (and (< (char-code char) 128)
       (or (alphanumericp char)
           (char= char #\-))))

(defun valid-ip-literal-p (domain)
  "Validates IP literal format per RFC 5321.
   
   Supports both IPv4 addresses and IPv6 with explicit prefix."
  (and (char= (char domain 0) #\[)
       (char= (char domain (1- (length domain))) #\])
       (let ((address-part (subseq domain 1 (1- (length domain)))))
         (or (ipv4-address-p address-part)
             (ipv6-literal-p address-part)))))

(defun ipv4-address-p (address)
  "Validates IPv4 dotted decimal notation."
  (let ((octets (split-on-character address #\.)))
    (and (= (length octets) 4)
         (every #'valid-ipv4-octet-p octets))))

(defun valid-ipv4-octet-p (octet)
  "IPv4 octets must be 0-255 decimal values."
  (and (> (length octet) 0)
       (<= (length octet) 3)
       (every #'digit-char-p octet)
       (let ((value (parse-integer octet)))
         (<= 0 value 255))))

(defun ipv6-literal-p (address)
  "Basic IPv6 literal validation with explicit prefix.
   
   Simplified implementation - full IPv6 parsing is complex
   and rarely needed in practice."
  (and (>= (length address) 5)
       (string= "IPv6:" (subseq address 0 5))
       (ipv6-address-format-p (subseq address 5))))

(defun ipv6-address-format-p (address)
  "Basic IPv6 address format check (hexadecimal and colons)."
  (and (position #\: address)
       (every #'valid-ipv6-character-p address)))

(defun valid-ipv6-character-p (char)
  "IPv6 addresses contain only hexadecimal digits and colons."
  (or (digit-char-p char 16)
      (char= char #\:)))

(defun domain-labels (domain)
  "Splits domain into labels on dot boundaries."
  (split-on-character domain #\.))

(defun split-on-character (string delimiter)
  "Utility function for splitting strings on delimiter character."
  (let ((parts nil)
        (start 0))
    (loop for position = (position delimiter string :start start)
          while position
          do (push (subseq string start position) parts)
             (setf start (1+ position))
          finally (push (subseq string start) parts))
    (nreverse parts)))