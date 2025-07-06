# Email Validator

**⚠️ Alpha Release - Work in Progress**

A RFC-compliant email validation library for Common Lisp.

## Features

- **RFC 5321 compliant** email validation
- Validates local part (before @) and domain part (after @)
- Supports special characters allowed by RFC
- Supports IP literals (IPv4 and IPv6)
- Enforces proper length limits:
  - Local part: max 64 characters
  - Domain labels: max 63 characters  
  - Total email: max 320 characters
- ASCII-only validation (no Unicode)
- Zero dependencies

## Installation

This library uses ASDF for system definition.

### Option 1: Manual Installation
1. Clone to a location where ASDF can find it:
   ```bash
   cd ~/common-lisp/  # or ~/quicklisp/local-projects/
   git clone <repository-url> email-validator
   ```

2. Load the system:
   ```lisp
   (asdf:load-system :email-validator)
   ```

### Option 2: Quicklisp (if published)
```lisp
(ql:quickload :email-validator)
```

## Usage

```lisp
;; Load the system
(asdf:load-system :email-validator)

;; Validate email addresses
(email-validator:email-p "user@example.com") ; => T

(email-validator:email-p "invalid.email")    ; => NIL
(email-validator:email-p "@invalid.com")     ; => NIL
(email-validator:email-p "user@")            ; => NIL

;; Advanced examples
(email-validator:email-p "user+tag@example.com")  ; => T
(email-validator:email-p "user@[192.168.1.1]")    ; => T
(email-validator:email-p "user@[IPv6:2001:db8::1]") ; => T
```

## API

### Functions

- `(email-p email)` → `boolean`

Returns `T` if the email is valid according to RFC specifications, `NIL` otherwise.

## Validation Rules

### Local Part (before @)
- Max 64 characters
- Allowed characters: alphanumeric + `. ! # $ % & ' * + - / = ? ^ _ ` { | } ~`
- Cannot start or end with a dot
- No consecutive dots
- ASCII only

### Domain Part (after @)
- Domain names: standard DNS rules
- IP literals: `[192.168.1.1]` or `[IPv6:2001:db8::1]`
- Labels max 63 characters each
- Must have at least 2 labels (e.g., `example.com`)
- ASCII only

### Overall
- Single @ symbol required
- Max 320 characters total
- Must be a string

## Running Tests

```lisp
;; Run all tests
(asdf:test-system :email-validator)

;; Or load and run directly
(asdf:load-system :email-validator/tests)
(email-validator.tests:run-tests)
```

The test suite includes 43 comprehensive tests covering valid emails, invalid emails, edge cases, and RFC compliance.

## Examples

See `example.lisp` for interactive examples and usage patterns.

## Goals

- Support for quoted strings in local parts (RFC 5321)
- Email address comments parsing
- Internationalized domain names (IDN) support
- Unicode/non-ASCII character validation
- Advanced RFC format compatibility
- Performance optimizations
- Extended API with detailed validation results
- Batch validation support
- Configurable validation strictness levels

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `(asdf:test-system :email-validator)`
5. Submit a pull request

## License

MIT License - see LICENSE file for details.

## Author

Created by mtha790
