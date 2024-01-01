;;; SPDX-FileCopyrightText: 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests guix-file)
  #:use-module (hall common)
  #:use-module (hall config)
  #:use-module (hall spec)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

(define has-guix?
  (false-if-exception (resolve-module '(guix))))

(define* (dummy-spec #:key
                     (features '())
                     (dependencies '`()))
  (scm->specification
   `(hall-description
     (name "dummy")
     (prefix "")
     (version "0.1.2")
     (author "Some One")
     (email "dummy@example.com")
     (copyright (2023))
     (synopsis "Dummy project")
     (description "This is a dummy description.")
     (home-page "https://example.com")
     (license gpl3+)
     (dependencies ,dependencies)
     (skip ())
     (features ,features)
     (files (libraries ())
            (tests ())
            (programs ())
            (documentation ())
            (infrastructure ())))))

(define (read-from-output thunk)
  "`read' the standard output of THUNK.  The result is enclosed in a
list to regroup potentially multiple distinct sexps."
  (call-with-input-string
      (call-with-output-string
        (lambda (port)
          (with-output-to-port port
            (lambda _
              ;; Enclose the output sexp(s) in a list, so that
              ;; everything gets read, not just the first sexp.
              (display "(" port)
              (thunk)
              (display ")" port)))))
    read))

(define* (guix-scm-sexp #:key (dependencies '`()))
  "Return the sexp of the computed 'guix.scm' file."
  (let ((spec (dummy-spec #:features `((guix #t)
                                       (use-guix-specs-for-dependencies
                                        ,(use-guix-specs-for-dependencies?)))
                          #:dependencies dependencies)))
    (read-from-output
     (lambda _ ((guix-file) spec '() 'show-contents "")))))

(define (guix-sexp->modules sexp)
  (match sexp
    ((('use-modules module ...) package-sexp ...)
     module)))

(define (contains-modules? sexp modules)
  "Predicate to check whether SEXP uses the Guile MODULES."
  (let ((m (guix-sexp->modules sexp)))
    (every (cut member <> m) modules)))

(test-begin "guix-file")

(test-assert "guix.scm file modules, old-style inputs, without guix specs"
  (parameterize ((use-guix-specs-for-dependencies? #f))
    (let ((sexp (guix-scm-sexp #:dependencies '`(("coreutils" ,coreutils)
                                                 ("xrandr" ,xrandr)))))
      ;; Before the introduction of the use-guix-specs-for-dependencies
      ;; feature, dependencies modules were not automatically added.
      (not (contains-modules? sexp '((gnu packages base)
                                     (gnu packages xorg)))))))

(unless has-guix? (test-skip 1))
(test-assert "guix.scm file modules, with guix spec inputs"
  (parameterize ((use-guix-specs-for-dependencies? #t))
    (let ((sexp (guix-scm-sexp #:dependencies '("coreutils"
                                                "xrandr"))))
      (contains-modules? sexp '((gnu packages base)
                                (gnu packages xorg))))))

(test-end "guix-file")
