;; tests/common.scm --- common implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2021 Alex Sassmannshausen <alex@pompo.co>
;; Copyright (C) 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;; Author: Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;
;; This file is part of guile-hall.
;;
;; guile-hall is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; guile-hall is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with guile-hall; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; Source-file: hall/common.scm
;;
;;; Code:

(define-module (tests common)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-71)
  #:use-module (hall config)
  #:use-module (hall common)
  #:use-module (hall spec))

(define has-guix?
  (false-if-exception (resolve-module '(guix))))

;;; For white-box testing:
(define dependencies->items
  (@@ (hall common) dependencies->items))

(define dependency->package+module
  (@@ (hall common) dependency->package+module))

(define guix-package->variable
  (@@ (hall common) guix-package->variable))

;;; XXX: Unfortunately, SRFI-64's test-error doesn't not match the
;;; error type, so we must use test-assert with the syntax below
;;; instead (see: https://bugs.gnu.org/66776).
(define-syntax-rule (with-expected-exception predicate body ...)
  "Return #t if an exception matched with PREDICATE was raised by
evaluating BODY, else #f."
  (guard (ex ((predicate ex) #t)
             (else #f))
    body ...
    #f))

(test-begin "common")

(test-begin "spec parsing")

(let ((spec '(hall-description
              (name "hall")
              (prefix "guile")
              (version "0.5.0")
              (author "Alex Sassmannshausen")
              (email "alex@komputilo.eu")
              (copyright (2018 2020 2021 2022 2023))
              (synopsis "Guile project tooling")
              (description
               "Hall is a command-line application and a set of Guile libraries that allow you to quickly create and publish Guile projects.  It allows you to transparently support the GNU build system, manage a project hierarchy & provides tight coupling to Guix.")
              (home-page
               "https://gitlab.com/a-sassmannshausen/guile-hall")
              (license gpl3+)
              (dependencies
               `(("guile-config" (config) ,guile-config)))
              (skip ())
              (features
               ((guix #t)
                (native-language-support #t)
                (licensing #t)))
              (files (libraries
                      ((directory
                        "hall"
                        ((hall-file "hconfig.scm")
                         (scheme-file "hconfig")
                         (scheme-file "config")
                         (scheme-file "publish")
                         (scheme-file "friends")
                         (scheme-file "workarounds")
                         (scheme-file "build")
                         (scheme-file "dist")
                         (scheme-file "common")
                         (scheme-file "scan")
                         (scheme-file "builders")
                         (scheme-file "clean")
                         (scheme-file "init")
                         (scheme-file "spec")))))
                     (tests ((directory
                              "tests"
                              ((scheme-file "build")
                               (scheme-file "scan")
                               (scheme-file "common")
                               (scheme-file "hall")
                               (scheme-file "spec")))))
                     (programs
                      ((directory "scripts" ((in-file "hall")))))
                     (documentation
                      ((org-file "README")
                       (symlink "README" "README.org")
                       (text-file "HACKING")
                       (text-file "COPYING")
                       (text-file "NEWS")
                       (text-file "AUTHORS")
                       (text-file "ChangeLog")
                       (directory "doc" ((texi-file "hall")))))
                     (infrastructure
                      ((unknown-type ".gitlab-ci.yml")
                       (scheme-file "guix")
                       (scheme-file "hall")))))))

;;;;; Tests for: href
  (let ((href (@@ (hall common) href)))
    (test-equal "href single-value"
      "guile"
      (href spec 'prefix))
    (test-assert "href not-found"
      (not (href spec 'gobbledygook)))
    (test-assert "href multiple-values"
      (list? (href spec 'files))))

;;;;; Tests for: merge-skip
  ;; (let ((merge-skip (@@ (hall common) merge-skip)))
  ;;   ((test-assert "placeholder-merge-skip"
  ;;      (boolean? #t))))

;;;;; Tests for: merge-skip-clean

  (let ((merge-skip-clean (@@ (hall common) merge-skip-clean))
        (spec (λ (skip)
                (scm->specification `(hall-description
                                      (name "minimal") (prefix "minimal")
                                      (version "0.0.1") (author "atheia")
                                      (email "test@example.com")
                                      (copyright (2023)) (synopsis "")
                                      (description "") (home-page "")
                                      (license gpl3+) (dependencies `())
                                      (skip ,skip)
                                      (files ((libraries)
                                              (tests)
                                              (programs)
                                              (documentation)
                                              (infrastructure))))))))
    (test-assert "CLEAN: Empty skip"
      (null? (merge-skip-clean (spec '()) '())))
    (test-assert "CLEAN: Empty clean skip"
      (null? (merge-skip-clean (spec '((clean))) '())))
    (test-assert "CLEAN: No clean skip"
      (null? (merge-skip-clean (spec '((scan "thuent" "tsuea"))) '())))
    (test-assert "CLEAN: Clean skip merge"
      (merge-skip-clean (spec '((scan "thuent" "tsuea"))) '("test")))
    (test-assert "CLEAN: Clean skip"
      (merge-skip-clean (spec '((clean "thuent" "tsuea"))) '())))

;;;;; Tests for: merge-skip-scan

  (let ((merge-skip-scan (@@ (hall common) merge-skip-scan))
        (spec (λ (skip)
                (scm->specification `(hall-description
                                      (name "minimal") (prefix "minimal")
                                      (version "0.0.1") (author "atheia")
                                      (email "test@example.com")
                                      (copyright (2023)) (synopsis "")
                                      (description "") (home-page "")
                                      (license gpl3+) (dependencies `())
                                      (skip ,skip)
                                      (files ((libraries)
                                              (tests)
                                              (programs)
                                              (documentation)
                                              (infrastructure))))))))
    (test-assert "SCAN: Empty skip"
      (null? (merge-skip-scan (spec '()) '())))
    (test-assert "SCAN: Empty scan skip"
      (null? (merge-skip-scan (spec '((scan))) '())))
    (test-assert "SCAN: No scan skip"
      (null? (merge-skip-scan (spec '((clean "thuent" "tsuea"))) '())))
    (test-assert "SCAN: Scan skip merge"
      (merge-skip-scan (spec '((scan "thuent" "tsuea"))) '("test")))
    (test-assert "SCAN: Scan skip"
      (merge-skip-scan (spec '((scan "thuent" "tsuea"))) '()))))

(test-end "spec parsing")

;;;;; Tests for: source-files

(test-begin "source-files")
(let ((source-files (@@ (hall common) source-files)))
  (test-assert "Only .in files"
    (null? (source-files '("test/x.in" "hall/x/test.in" "blah.in"))))
  (test-assert "Only .hall files"
    (null? (source-files '("test/x.hall" "hall/x/test.hall" "blah.hall"))))
  (test-assert "Only .hall and .in files"
    (null? (source-files '("test/x.hall" "hall/x/test.in" "blah.hall"
                           "foo.in"))))
  (test-assert "Mixed files"
    (equal? '("test/x.scm" "blah.hall.scm" )
            (source-files '("test/x.hall" "test/x.scm" "hall/x/test.in"
                            "blah.hall.scm" "hall/x/test.scm.in" "blah.hall"
                            "foo.in")))))
(test-end "source-files")

;;;;; Tests for: file->filepath
(test-begin "file->filepath")

(test-equal "scheme-file resolution"
  "tests/test.scm"
  ((@@ (hall common) file->filepath) 'scheme-file "test" '("tests")))

(test-equal "unknown file resolution"
  "tests/test"
  ((@@ (hall common) file->filepath) 'eduiranedu "test" '("tests")))

(test-end "file->filepath")

;;;;; Tests for: filetype-read
(test-begin "filetype-read")

(test-equal "scheme-file read"
  "tests/test.scm"
  ((filetype-read 'scheme-file "test" "tests/test.scm" (make-hash-table)) '()
   '("tests") 'path ""))

(test-equal "unknown-file read"
  "tests/test"
  ((filetype-read 'unknown-type "test" "tests/test.edurinaed"
                  (make-hash-table)) '() '("tests") 'path ""))

;;;; Tests for dependency validation.
(test-assert "dependencies->items, old-style, invalid dependencies field"
  (with-expected-exception
   invalid-dependencies-field?
   (dependencies->items '("expected" "quasiquoted" "list"))))

(test-equal "dependencies->items, old-style, valid dependency field"
  '(("guile-hall" (hall common) ,guile-hall)
    ("guix" ,guix))
  (dependencies->items '`(("guile-hall" (hall common) ,guile-hall)
                          ("guix" ,guix))))

(test-equal "dependencies->items, old-style, empty list"
  '()
  (dependencies->items '`()))

(unless has-guix? (test-skip 1))
(test-equal "dependencies->items, new-style, valid dependency field"
  '(("guile-hall" (hall common))
    "guix")
  (parameterize ((use-guix-specs-for-dependencies? #t))
    (dependencies->items '(("guile-hall" (hall common))
                           "guix"))))

(test-equal "dependencies->items, new-style, empty list"
  '()
  (parameterize ((use-guix-specs-for-dependencies? #t))
    (dependencies->items '())))

(test-equal "old-inputs style"
  'guile-hall-2
  (dependency->package+module '("guile-hall" ,guile-hall-2)))

(test-equal "old-inputs style, with module symbol"
  '(guile-hall-2 (hall common))
  (let ((variable module (dependency->package+module
                      '("guile-hall" (hall common) ,guile-hall-2))))
    (list variable module)))

(test-equal "old-inputs style, without module symbol"
  '(guile-hall #f)
  (let ((variable module (dependency->package+module
                      '("label" ,guile-hall))))
    (list variable module)))

(test-assert "old-inputs style, invalid input"
  (with-expected-exception
   invalid-dependency-input?
   (dependency->package+module
    '("guix" "specs" "but" "expected" "old" "style"))))

(unless has-guix? (test-skip 1))
(test-assert "guix specs dependencies, invalid input"
  (with-expected-exception
   invalid-dependency-input?
   (parameterize ((use-guix-specs-for-dependencies? #t))
     (dependency->package+module
      ;; Old-style provided, expected Guix specifications.
      '("guile-git" ,guile-git)))))

(unless has-guix? (test-skip 1))
(test-assert "guix specs dependencies, unresolved spec"
  (with-expected-exception
   invalid-guix-input-specification?
   (parameterize ((use-guix-specs-for-dependencies? #t))
     (dependency->package+module "nonexistent/package:doc@9999.1.8"))))

(unless has-guix? (test-skip 1))
(test-equal "guix specs dependencies, with module symbol"
  '(guile-hall (hall common))
  (parameterize ((use-guix-specs-for-dependencies? #t))
    (let ((package module (dependency->package+module
                           '("guile-hall" (hall common)))))
      (list (guix-package->variable package) module))))

(unless has-guix? (test-skip 1))
(test-equal "guix specs dependencies, without module symbol"
  '(guile-hall #f)
  (parameterize ((use-guix-specs-for-dependencies? #t))
    (let ((package module (dependency->package+module "guile-hall")))
      (list (guix-package->variable package) module))))

(test-end "filetype-read")

(test-end "common")
