;; hall/common.scm --- common implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2018-2020 Alex Sassmannshausen <alex@pompo.co>
;; Copyright (C) 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
;;; Code:

(define-module (hall common)
  #:use-module (config licenses)
  #:use-module (hall config)
  #:use-module (hall spec)
  #:use-module (hall builders)

  ;; From Guix.
  #:autoload (gnu packages) (specification->package)
  #:autoload (guix diagnostics) (location-file)
  #:autoload (guix modules) (file-name->module-name)
  #:autoload (guix packages) (package-location package-name)

  #:use-module (ice-9 control)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web http)
  #:re-export (project-root-directory?
               find-project-root-directory find-project-root-directory*
               expand-filename part-of-project?
               quit-with-error
               G_ N_)
  #:export (default-files
            instantiate

            blacklisted?

            read-features read-spec filetype-read category-traverser
            scm->files scm->specification

            guix-file brew-file hconfig-file
            invalid-dependencies-field?
            invalid-dependency-input?
            invalid-guix-input-specification?
            guix-package-variable-not-found?

            hall-template-file

            base-autotools
            base-autotools-documentation base-autotools-infrastructure
            flatten merge-skip merge-skip-clean merge-skip-scan))

(define (default-files project-name)
  (files (base-libraries project-name)
         (base-tests)
         (base-programs)
         (base-documentation project-name)
         (base-infrastructure)))

(define (instantiate spec context operation)
  "Carry out the operation OPERATION, a symbol that should be 'show or 'exec,
against the projec described by the specification SPEC in the list describing
the folder location CONTEXT."
  (for-each (cute <> spec context operation "  ")
            (apply append
                   (map (cute <> (specification-files spec))
                        (list files-libraries files-tests files-programs
                              files-documentation files-infrastructure)))))

(define* (blacklisted? path project-root skip #:optional conservative?)
  "Return #t if the absolute filepath PATH, located in the project at absolute
filepath PROJECT-ROOT is contained in the list of relative file-paths SKIP."
  (and (not (string=? project-root path))
       (let ((file (string-drop path (1+ (string-length project-root)))))
         (catch 'X
           (λ _
             (for-each (λ (p) (and (string-match p file) (throw 'X)))
                       (append (if conservative?
                                   '()
                                   '("^\\.dir-locals.el" "^\\.gitignore$"))
                               (cons* "^.*~$" "^#.*#$" "^\\.git$" skip)))
             #f)
           (const #t)))))

(define-exception-type &invalid-dependencies-field
  &error                                ;parent
  make-invalid-dependencies-field       ;constructor
  invalid-dependencies-field?           ;predicate
  (sexp invalid-dependencies-field-sexp))

(define-exception-type &invalid-dependency-input
  &error                                ;parent
  make-invalid-dependency-input         ;constructor
  invalid-dependency-input?             ;predicate
  (sexp invalid-dependency-input-sexp))

(define-exception-type &invalid-guix-input-specification
  &error
  make-invalid-guix-input-specification ;constructor
  invalid-guix-input-specification?     ;predicate
  (text invalid-guix-input-specification-text))

(define-exception-type &guix-package-variable-not-found
  &error
  make-guix-package-variable-not-found ;constructor
  guix-package-variable-not-found?     ;predicate
  (name guix-package-variable-not-found-name))

;;; XXX: Adapted from 'package->variable' from the
;;; hydra/build-package-metadata.scm module from the guix-maintenance
;;; repository (see:
;;; https://git.savannah.gnu.org/cgit/guix/maintenance.git).
(define (guix-package->variable package)
  "Return the name of the variable whose value is PACKAGE in the module that
defines it, else raise a "
  (match (package-location package)
    (#f (raise-exception (make-guix-package-variable-not-found
                          (package-name package))))
    ((= location-file file)
     (let* ((name (file-name->module-name file))
            (module (false-if-exception (resolve-interface name))))
       (let/ec return
         (module-for-each (lambda (symbol variable)
                            (when (eq? package (variable-ref variable))
                              (return symbol)))
                          module)
         (raise-exception (make-guix-package-variable-not-found
                           (package-name package))))))))

(define (dependencies->items dependencies)
  "Normalize the list of dependency items, stripping the quasiquote if
using old-style Guix inputs.  If DEPENDENCIES does not match the
expected syntax, an &invalid-dependencies-field exception is raised."
  (if (use-guix-specs-for-dependencies?)
      (match dependencies
        ((item ...)
         item)
        (_ (raise-exception
            (make-invalid-dependencies-field dependencies))))
      (match dependencies
        (('quasiquote ()) '())
        (('quasiquote (item ...))
         item)
        (_ (raise-exception
            (make-invalid-dependencies-field dependencies))))))

(define (dependency->package+module item)
  "Return the Guix package object corresponding to ITEM, a Hall
dependency item if the USE-GUIX-SPECS-FOR-DEPENDENCIES? is #t, else
the package *variable*, a symbol.  If the input format is unexpected, an
&invalid-dependency-input error is raised.  A second value contains
the Guile module, e.g. '(hall common), when specified via the Hall
augmented input format is used, else #f.  If the
USE-GUIX-SPECS-FOR-DEPENDENCIES?  parameter is #t, the Guix package
specification is validated, and an &invalid-guix-input-specification
exception is raised when the specification could not match any Guix
package."
  (define (guix-spec->package guix-spec)
    ;; XXX: 'specification->package' calls exit in case of error.
    (guard (ex ((quit-exception? ex)
                (raise-exception
                 (make-invalid-guix-input-specification guix-spec))))
      (specification->package guix-spec)))

  (if (use-guix-specs-for-dependencies?)
      (match item
        ((? string? guix-spec)
         (values (guix-spec->package guix-spec) #f))
        (((? string?) ('unquote . _))
         ;; Expected Guix specifications, got old style inputs.
         (raise-exception (make-invalid-dependency-input item)))
        (((? string? guix-spec) ((? symbol? module) ...))
         ;; For augmented Hall input declaration.
         (values (guix-spec->package guix-spec) module))
        (_ (raise-exception (make-invalid-dependency-input item))))
      (match item
        ;; Old-style Guix inputs.
        (((? string? label) ('unquote (? symbol? package-variable)))
         (values package-variable #f))
        ;; Augmented Hall input declaration with Guile module symbol.
        (((? string? label) ((? symbol? module) ...)
          ('unquote (? symbol? package-variable)))
         (values package-variable module))
        (_ (raise-exception (make-invalid-dependency-input item))))))

(define (read-features)
  "Read the features only from the project's hall.scm file."
  (find-project-root-directory)
  (scm->features
   (with-input-from-file "hall.scm"
     (lambda _ (read)))))

(define* (read-spec #:optional abs-file)
  "Set the working directory to the current project's root directory & parse
the project's hall.scm file.

When abs-file is given, return the spec in ABS-FILE as scm."
  (if abs-file
      (begin
        (chdir (dirname abs-file))
        (with-input-from-file abs-file (λ _ (read))))
      (begin
        (find-project-root-directory)
        (scm->specification
         (with-input-from-file "hall.scm"
           (lambda _ (read)))))))

;;;; Defaults

(define (base-libraries name)
  "Return the default libraries section."
  `(,(file name scheme-filetype "")
    ,(directory name `(,(hconfig-file)))))

(define (base-tests)
  "Return the default tests section."
  `(,(directory "tests" '())))

(define (base-programs)
  "Return the default programs section."
  `(,(directory "scripts" `())))

(define (base-top-docs)
  "Return the default top level documentation section."
  `(,(file "README" org-filetype
           (lambda (spec)
             (format #t
                     "# -*- mode: org; coding: utf-8; -*-

#+TITLE: README for ~a~%~%"
                     (friendly-project-name spec))))
    ,(slink "README" "README.org")
    ,(file "HACKING" text-filetype
           (lambda (spec)
             (format #t
                     "# -*- mode: org; coding: utf-8; -*-

#+TITLE: Hacking ~a

* Contributing

By far the easiest way to hack on ~a is to develop using Guix:

#+BEGIN_SRC bash
  # Obtain the source code
  cd /path/to/source-code
  guix shell -Df guix.scm
  # In the new shell, run:
  hall build --execute && autoreconf -vif && ./configure && make check
#+END_SRC

You may also want to set your directory as an authorized directory for
`guix shell' so it works without arguments. To do that, simply run

#+BEGIN_SRC bash
  echo $(pwd) >> $HOME/.config/guix/shell-authorized-directories
#+END_SRC

You can now hack this project's files to your heart's content, whilst
testing them from your `guix shell' shell.

To try out any scripts in the project you can now use

#+BEGIN_SRC bash
  ./pre-inst-env scripts/${script-name}
#+END_SRC

If you'd like to tidy the project again, but retain the ability to test the
project from the commandline, simply run:

#+BEGIN_SRC bash
  ./hall clean --skip \"scripts/${script-name},pre-inst-env\" --execute
#+END_SRC

** Manual Installation

If you do not yet use  Guix, you will have to install this project's
dependencies manually:
  - autoconf
  - automake
  - pkg-config
  - texinfo
  - guile-hall~a

Once those dependencies are installed you can run:

#+BEGIN_SRC bash
  hall build -x && autoreconf -vif && ./configure && make check
#+END_SRC
"
                     (specification-name spec) (specification-name spec)
                     (string-join
                      (let ((packages (map dependency->package+module
                                           (dependencies->items
                                            (specification-dependencies
                                             spec)))))
                        (if (use-guix-specs-for-dependencies?)
                            (map package-name packages)
                            (map symbol->string packages)))

                      "\n  - " 'prefix))))
    ,(file "COPYING" text-filetype
           (lambda (spec)
             (match (specification-license spec)
               ((? defined? n)
                (fetch-license (eval (specification-license spec)
                                     (interaction-environment))
                               #t))
               (sym (format #t "This project's license is ~a.~%" sym)))))))

(define (fetch-license license online?)
  "Return a basic license information string for LICENSE.  If LICENSE is part
of the GPL family, and we are ONLINE?, fetch the full license instead."
  (define (fetch uri)
    (catch #t
      (lambda _
        (call-with-values
            (lambda _
              (http-get uri))
          (lambda (response body)
            (match (response-code response)
              (200 (display body))
              ((or 301 302)
               (fetch (assq-ref (response-headers response) 'location)))
              (_
               (fetch-license license #f))))))
      (lambda (k . args)
        (fetch-license license #f))))
  (if (and online?
           (string-match "^[AL]{0,1}GPL [1-3].*$" (license-name license)))
      (fetch (regexp-substitute #f (string-match "\\.html$"
                                                 (license-uri license))
                                'pre ".txt"))
      (format #t "This project's license is ~a.~%
You can read the full license at ~a.~%"
              (license-name license)
              (license-uri license))))

(define (base-documentation name)
  "Return the complete default documentation section."
  `(,@(base-top-docs)
    ,(directory "doc"
                `(,(manual-file name)))))

(define (base-infrastructure)
  "Return the default infrastructure section."
  `(,(guix-file)
    ,(file ".gitignore" text-filetype
           "*.eps
*.go
*.log
*.pdf
*.png
*.tar.xz
*.tar.gz
*.tmp
*~
.#*
\\#*\\#
,*
/ABOUT-NLS
/INSTALL
/aclocal.m4
/autom4te.cache
/build-aux/ar-lib
/build-aux/compile
/build-aux/config.guess
/build-aux/config.rpath
/build-aux/config.sub
/build-aux/depcomp
/build-aux/install-sh
/build-aux/mdate-sh
/build-aux/missing
/build-aux/test-driver
/build-aux/texinfo.tex
/config.status
/configure
/doc/*.1
/doc/.dirstamp
/doc/contributing.*.texi
/doc/*.aux
/doc/*.cp
/doc/*.cps
/doc/*.fn
/doc/*.fns
/doc/*.html
/doc/*.info
/doc/*.info-[0-9]
/doc/*.ky
/doc/*.pg
/doc/*.toc
/doc/*.t2p
/doc/*.tp
/doc/*.vr
/doc/*.vrs
/doc/stamp-vti
/doc/version.texi
/doc/version-*.texi
/m4/*
/pre-inst-env
/test-env
/test-tmp
/tests/*.trs
GPATH
GRTAGS
GTAGS
Makefile
Makefile.in
config.cache
stamp-h[0-9]
tmp
/.version
/doc/stamp-[0-9]
")
    ,(file "hall" scheme-filetype #f)))

(define (base-autotools-documentation)
  "Return the default autotools documentation section."
  `(,(file "NEWS" text-filetype
           (lambda (spec)
             (format #t
                     "# -*- mode: org; coding: utf-8; -*-

#+TITLE: ~a NEWS – history of user-visible changes
#+STARTUP: content hidestars

Copyright © ~a ~a <~a>

  Copying and distribution of this file, with or without modification,
  are permitted in any medium without royalty provided the copyright
  notice and this notice are preserved.

Please send ~a bug reports to ~a.

* Publication at ~a~%"
                     (friendly-project-name spec)
                     (specification-copyright spec)
                     (specification-author spec)
                     (specification-email spec)
                     (friendly-project-name spec)
                     (specification-email spec)
                     (specification-version spec))))
    ,(file "AUTHORS" text-filetype
           (lambda (spec)
             (format #t
                     "Contributors to ~a ~a:

    ~a <~a>~%"
                     (friendly-project-name spec) (specification-version spec)
                     (specification-author spec) (specification-email spec))))
    ,(file "ChangeLog" text-filetype
           (lambda (spec)
             (format #t
                     "For a complete log, please see the Git commit log at <~a/PATH/TO/LOG>.~%"
                     (specification-home-page spec))))))


(define (base-autotools-infrastructure)
  "Return the default autotools section."
  (filter identity
          `(,(directory "build-aux"
                        `(,(file "test-driver" scheme-filetype
                                 ";;;; test-driver.scm - Guile test driver for Automake testsuite harness

(define script-version \"2019-01-15.13\") ;UTC

;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2019 Alex Sassmannshausen <alex@pompo.co>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:
;;;
;;; This script provides a Guile test driver using the SRFI-64 Scheme API for
;;; test suites.  SRFI-64 is distributed with Guile since version 2.0.9.
;;;
;;; This script is a lightly modified version of the orignal written by
;;; Matthieu Lirzin.  The changes make it suitable for use as part of the
;;; guile-hall infrastructure.
;;;
;;;; Code:

(use-modules (ice-9 getopt-long)
             (ice-9 pretty-print)
             (srfi srfi-26)
             (srfi srfi-64))

(define (show-help)
  (display \"Usage:
                         test-driver --test-name=NAME --log-file=PATH --trs-file=PATH
                         [--expect-failure={yes|no}] [--color-tests={yes|no}]
                         [--enable-hard-errors={yes|no}] [--brief={yes|no}}] [--]
               TEST-SCRIPT [TEST-SCRIPT-ARGUMENTS]
The '--test-name', '--log-file' and '--trs-file' options are mandatory.\n\"))

(define %options
  '((test-name                  (value #t))
    (log-file                   (value #t))
    (trs-file                   (value #t))
    (color-tests                (value #t))
    (expect-failure             (value #t)) ;XXX: not implemented yet
    (enable-hard-errors         (value #t)) ;not implemented in SRFI-64
    (brief                      (value #t))
    (help    (single-char #\\h) (value #f))
    (version (single-char #\\V) (value #f))))

(define (option->boolean options key)
  \"Return #t if the value associated with KEY in OPTIONS is 'yes'.\"
  (and=> (option-ref options key #f) (cut string=? <> \"yes\")))

(define* (test-display field value  #:optional (port (current-output-port))
                       #:key pretty?)
  \"Display 'FIELD: VALUE\\n' on PORT.\"
  (if pretty?
      (begin
        (format port \"~A:~%\" field)
        (pretty-print value port #:per-line-prefix \"+ \"))
      (format port \"~A: ~S~%\" field value)))

(define* (result->string symbol #:key colorize?)
  \"Return SYMBOL as an upper case string.  Use colors when COLORIZE is #t.\"
  (let ((result (string-upcase (symbol->string symbol))))
    (if colorize?
        (string-append (case symbol
                         ((pass)       \"[0;32m\")  ;green
                         ((xfail)      \"[1;32m\")  ;light green
                         ((skip)       \"[1;34m\")  ;blue
                         ((fail xpass) \"[0;31m\")  ;red
                         ((error)      \"[0;35m\")) ;magenta
                       result
                       \"[m\")          ;no color
        result)))

(define* (test-runner-gnu test-name #:key color? brief? out-port trs-port)
  \"Return an custom SRFI-64 test runner.  TEST-NAME is a string specifying the
file name of the current the test.  COLOR? specifies whether to use colors,
and BRIEF?, well, you know.  OUT-PORT and TRS-PORT must be output ports.  The
current output port is supposed to be redirected to a '.log' file.\"

  (define (test-on-test-begin-gnu runner)
    ;; Procedure called at the start of an individual test case, before the
    ;; test expression (and expected value) are evaluated.
    (let ((result (cute assq-ref (test-result-alist runner) <>)))
      (format #t \"test-name: ~A~%\" (result 'test-name))
      (format #t \"location: ~A~%\"
              (string-append (result 'source-file) \":\"
                             (number->string (result 'source-line))))
      (test-display \"source\" (result 'source-form) #:pretty? #t)))

  (define (test-on-test-end-gnu runner)
    ;; Procedure called at the end of an individual test case, when the result
    ;; of the test is available.
    (let* ((results (test-result-alist runner))
           (result? (cut assq <> results))
           (result  (cut assq-ref results <>)))
      (unless brief?
        ;; Display the result of each test case on the console.
        (format out-port \"~A: ~A - ~A~%\"
                (result->string (test-result-kind runner) #:colorize? color?)
                test-name (test-runner-test-name runner)))
      (when (result? 'expected-value)
        (test-display \"expected-value\" (result 'expected-value)))
      (when (result? 'expected-error)
        (test-display \"expected-error\" (result 'expected-error) #:pretty? #t))
      (when (result? 'actual-value)
        (test-display \"actual-value\" (result 'actual-value)))
      (when (result? 'actual-error)
        (test-display \"actual-error\" (result 'actual-error) #:pretty? #t))
      (format #t \"result: ~a~%\" (result->string (result 'result-kind)))
      (newline)
      (format trs-port \":test-result: ~A ~A~%\"
              (result->string (test-result-kind runner))
              (test-runner-test-name runner))))

  (define (test-on-group-end-gnu runner)
    ;; Procedure called by a 'test-end', including at the end of a test-group.
    (let ((fail (or (positive? (test-runner-fail-count runner))
                    (positive? (test-runner-xpass-count runner))))
          (skip (or (positive? (test-runner-skip-count runner))
                    (positive? (test-runner-xfail-count runner)))))
      ;; XXX: The global results need some refinements for XPASS.
      (format trs-port \":global-test-result: ~A~%\"
              (if fail \"FAIL\" (if skip \"SKIP\" \"PASS\")))
      (format trs-port \":recheck: ~A~%\"
              (if fail \"yes\" \"no\"))
      (format trs-port \":copy-in-global-log: ~A~%\"
              (if (or fail skip) \"yes\" \"no\"))
      (when brief?
        ;; Display the global test group result on the console.
        (format out-port \"~A: ~A~%\"
                (result->string (if fail 'fail (if skip 'skip 'pass))
                                #:colorize? color?)
                test-name))
      #f))

  (let ((runner (test-runner-null)))
    (test-runner-on-test-begin! runner test-on-test-begin-gnu)
    (test-runner-on-test-end! runner test-on-test-end-gnu)
    (test-runner-on-group-end! runner test-on-group-end-gnu)
    (test-runner-on-bad-end-name! runner test-on-bad-end-name-simple)
    runner))

;;;
;;; Entry point.
;;;

(define (main . args)
  (let* ((opts   (getopt-long (command-line) %options))
         (option (cut option-ref opts <> <>)))
    (cond
     ((option 'help #f)    (show-help))
     ((option 'version #f) (format #t \"test-driver.scm ~A\" script-version))
     (else
      (let ((log (open-file (option 'log-file \"\") \"w0\"))
            (trs (open-file (option 'trs-file \"\") \"wl\"))
            (out (duplicate-port (current-output-port) \"wl\")))
        (redirect-port log (current-output-port))
        (redirect-port log (current-warning-port))
        (redirect-port log (current-error-port))
        (test-with-runner
            (test-runner-gnu (option 'test-name #f)
                             #:color? (option->boolean opts 'color-tests)
                             #:brief? (option->boolean opts 'brief)
                             #:out-port out #:trs-port trs)
          (load-from-path (option 'test-name #f)))
        (close-port log)
        (close-port trs)
        (close-port out))))
    (exit 0)))
" #t)))
            ,(configure-file)
            ,(and (nls-feature?)
                  (directory "po"
                             `(,(makevars-file)
                               ,(potfiles))))
            ,(makefile-file)
            ,(file "pre-inst-env" in-filetype
                   "#!/bin/sh

abs_top_srcdir=\"`cd \"@abs_top_srcdir@\" > /dev/null; pwd`\"
abs_top_builddir=\"`cd \"@abs_top_builddir@\" > /dev/null; pwd`\"

GUILE_LOAD_COMPILED_PATH=\"$abs_top_builddir${GUILE_LOAD_COMPILED_PATH:+:}$GUILE_LOAD_COMPILED_PATH\"
GUILE_LOAD_PATH=\"$abs_top_builddir:$abs_top_srcdir${GUILE_LOAD_PATH:+:}:$GUILE_LOAD_PATH\"
export GUILE_LOAD_COMPILED_PATH GUILE_LOAD_PATH

PATH=\"$abs_top_builddir/scripts:$PATH\"
export PATH

exec \"$@\"
" #t))))

(define (base-autotools)
  "Return the complete autotools section."
  `(,@(base-autotools-documentation)
    ,@(base-autotools-infrastructure)))

;;;;; Files

(define (manual-file name)
  "Return a hall file procedure with default contents for the project's
manual."
  (file name texi-filetype
        (lambda (spec)
          (display
           (string-append "\\input texinfo
@c -*-texinfo-*-

@c %**start of header
@setfilename " (full-project-name spec) ".info
@documentencoding UTF-8
@settitle " (friendly-project-name spec) " Reference Manual
@c %**end of header

@include version.texi

@copying
Copyright @copyright{} " (string-join (map number->string
                                           (specification-copyright spec))
                                      ", ") " " (specification-author spec) "

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with no
Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.  A
copy of the license is included in the section entitled ``GNU Free
Documentation License''.
@end copying

@dircategory The Algorithmic Language Scheme
@direntry
* " (friendly-project-name spec) ": (" (full-project-name spec) ").   " (specification-synopsis spec) "
@end direntry

@titlepage
@title The " (friendly-project-name spec) " Manual
@author " (specification-author spec) "

@page
@vskip 0pt plus 1filll
Edition @value{EDITION} @*
@value{UPDATED} @*

@insertcopying
@end titlepage

@contents

@c *********************************************************************
@node Top
@top " (friendly-project-name spec) "

This document describes " (friendly-project-name spec) " version @value{VERSION}.

@menu
* Introduction::                Why " (friendly-project-name spec) "?
@end menu

@c *********************************************************************
@node Introduction
@chapter Introduction

INTRODUCTION HERE

This documentation is a stub.

@bye
")))))

(define (potfiles)
  (file "POTFILES" in-filetype
        (lambda (spec)
          (format #t "# List of source files which contain translatable strings.
~a
" (string-join
   (filter (λ (filename)                ; Remove special hconfig.scm filename
             (and (not (string-match "^.+/hconfig.scm$" filename))
                  filename))
           (flatten
            (map (cut <> spec '() 'raw "") ; Return filename relative to project
                 (files-libraries (specification-files spec))))) "\n")))
        #t))

(define (makevars-file)
  (file "Makevars" text-filetype
        (lambda (spec)
          (format #t "DOMAIN = ~a
subdir = po
top_builddir = ..
XGETTEXT_OPTIONS =				\\
  --from-code=UTF-8				\\
  --keyword=G_ --keyword=N_:1,2			\\
  --keyword=n_
COPYRIGHT_HOLDER = ~a
PACKAGE_GNU = no
MSGID_BUGS_ADDRESS = ~a
EXTRA_LOCALE_CATEGORIES =
USE_MSGCTXT = no
MSGMERGE_OPTIONS = --previous
MSGINIT_OPTIONS =
PO_DEPENDS_ON_POT = yes
DIST_DEPENDS_ON_UPDATE_PO = yes
"
                  (full-project-name spec) (specification-author spec)
                  (specification-email spec)))
        #t))

(define* (AC_CONFIG_FILES file #:key executable?)
  "Generate an AC_CONFIG_FILES directive for FILE.  If EXECUTABLE? is
true, include the directive to have the generated file executable."
  (let ((file (regexp-substitute #f (string-match "\\.in$" file) 'pre)))
    (apply string-append
           `("AC_CONFIG_FILES([" ,file "]"
             ,@(if executable?
                   (list ",[chmod +x " file "]")
                   '())
             ")"))))

(define (input-file? file)
  "Predicate to check whether FILE is an input file (with file extension
\".in\")."
  (string-suffix? ".in" file))

(define (hall-file? file)
  "Predicate to check whether FILE is a Hall file (with file extension
\".hall\")."
  (string-suffix? ".hall" file))

(define (source-files files)
  (remove (λ (file-name) (any (cut <> file-name) (list input-file? hall-file?)))
          (flatten files)))

(define (configure-file)
  "Return a hall file procedure with default contents for the project's
configure.ac file."
  (file "configure" autoconf-filetype
        (lambda (spec)
          (define core-file (string-append (specification-name spec)
                                           ".scm"))
          (define spec-files (specification-files spec))
          (define program-files (files-programs spec-files))
          (define library-files (files-libraries spec-files))
          (define documentation-files (files-documentation spec-files))
          (define filter/input-files
            (lambda (x)
              (filter input-file?
                      (flatten (map (cut <> '() '() 'raw "")
                                    x)))))
          (define program-input-files (filter/input-files program-files))
          (define library-input-files (filter/input-files library-files))
          (define documentation-input-files
            (filter/input-files documentation-files))
          (display
           (string-append "dnl -*- Autoconf -*-

AC_INIT(" (full-project-name spec) ", " (specification-version spec) ")
AC_SUBST(HVERSION, \"\\\"" (specification-version spec) "\\\"\")
AC_SUBST(AUTHOR, \"\\\"" (specification-author spec) "\\\"\")
AC_SUBST(COPYRIGHT, \"'" (object->string (specification-copyright spec)) "\")
AC_SUBST(LICENSE, " (symbol->string (specification-license spec)) ")
AC_CONFIG_SRCDIR(" (if (file-exists? core-file)
                       core-file
                       (match (find (match-lambda (('directory . rest) #t)
                                                  (_ #f))
                                    (map (cut <> '() '() 'write "")
                                         (append program-files
                                                 library-files)))
                         (('directory name children) name)
                         (#f (quit-with-error "\
failed to locate a unique source file for AC_CONFIG_SRCDIR")))) ")
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])
"
(if (nls-feature?)
    "AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.21])
"
    "")

"
AC_CONFIG_FILES([Makefile" (if (nls-feature?) " po/Makefile.in" "") "])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
"
(string-join
 (append (map (cut AC_CONFIG_FILES <> #:executable? #t) program-input-files)
         (map AC_CONFIG_FILES library-input-files)
         (map AC_CONFIG_FILES documentation-input-files))
 "\n")
"
dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR
if test \"x$GUILD\" = \"x\"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

if test \"$cross_compiling\" != no; then
   GUILE_TARGET=\"--target=$host_alias\"
   AC_SUBST([GUILE_TARGET])
fi

dnl Hall auto-generated guile-module dependencies
"
                           (string-join
                            (let ((items (dependencies->items
                                          (specification-dependencies
                                           spec))))
                              (filter-map
                               (lambda (i)
                                 (let ((_ module (dependency->package+module
                                                  i)))
                                   (and module
                                        (string-append
                                         "GUILE_MODULE_REQUIRED(["
                                         (string-join
                                          (map symbol->string module))
                                         "])"))))
                               items))
                            "\n")
                           "

dnl Installation directories for .scm and .go files.
guilemoduledir=\"${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION\"
guileobjectdir=\"${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache\"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_OUTPUT
"))) #t))

;;;; Full on cargo cult!
(define (makefile-file)
  "Return a hall file procedure with default contents for the project's
Makefile.am file."
  (define (align elements padding)
    (match elements
      (() '())
      ((1st . rest)
       (cons 1st
             (map (lambda (element)
                    (string-append
                     (string-join (map (const " ") (iota padding)) "")
                     element))
                  rest)))))
  (file
   "Makefile" automake-filetype
   (lambda (spec)
     (display
      (string-append "bin_SCRIPTS = " (string-join
                                       (align
                                        (map (lambda (file)
                                               (or (and=> (string-match "\\.in$" file)
                                                          (cut regexp-substitute #f <> 'pre))
                                                   file))
                                             (flatten
                                              (map (cute <> spec '() 'raw "")
                                                   (files-programs (specification-files spec)))))
                                        14)
                                       " \\\n") "

nodist_noinst_SCRIPTS = pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_dist_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_dist_modDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o \"$@\" \"$<\"

SOURCES = "
(string-join
 (align
  (source-files (map (cut <> spec '() 'raw "")
                     ;; Return filename relative to project
                     (files-libraries (specification-files spec))))
  10)
 " \\\n") "

TESTS = " (string-join
           (align
            (flatten (map (cute <> spec '() 'raw "")
                          (files-tests (specification-files spec))))
            8)
           " \\\n") "

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \\
  $(top_builddir)/pre-inst-env                  \\
  $(GUILE) --no-auto-compile -e main            \\
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L \"$(top_srcdir)\"

AM_TESTS_ENVIRONMENT = abs_top_srcdir=\"$(abs_top_srcdir)\"

info_TEXINFOS = " (string-join
                   (align
                    (filter (cut string-match ".*\\.texi$" <>)
                            (flatten
                             (map (cute <> spec '() 'raw "")
                                  (files-documentation
                                   (specification-files spec)))))
                    16)
                   " \\\n") "

EXTRA_DIST = " (string-join
                (align
                 (append
                  (filter (negate (cut string-match ".*\\.texi$" <>))
                          (flatten
                           (map (cute <> spec '() 'raw "")
                                (files-documentation
                                 (specification-files spec)))))
                  (flatten (map (cute <> spec '() 'raw "")
                                (files-infrastructure
                                 (specification-files spec)))))
                 13)
                " \\\n") " \\
             build-aux/test-driver.scm \\
             $(TESTS)

ACLOCAL_AMFLAGS = -I m4
"
                 (if (nls-feature?) "SUBDIRS = po" "")
"
AM_DISTCHECK_DVI_TARGET = info # Disable DVI as part of distcheck

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\\
  $(BUILT_SOURCES)				\\
  $(GOBJECTS)					\\
  $(TESTS:tests/%.scm=%.log)
"))) #t))

(define (guix-wrap-binaries spec)
  "Return a guix arguments section, which, if spec has binaries, wraps each
binary in such a way that it can be run without additional dependencies being
installed in a profile."
  (define dep-labels
    (let ((items (dependencies->items (specification-dependencies spec))))
      (if (use-guix-specs-for-dependencies?)
          ;; XXX: The automatic labels added on Guix new style inputs
          ;; are derived from the package name, but these don't take
          ;; account non-standard outputs that may be specified in a
          ;; Guix package spec, so it doesn't allow differentiating
          ;; between multiple outputs at this time.
          (let ((packages (map dependency->package+module items)))
            (map package-name packages))
          (map first items))))

  (define guile-dep-labels
    (filter (cut string-prefix? "guile-" <>) dep-labels))

  (match (files-programs (specification-files spec))
    ;; No binaries, so arguments is just `()
    (() ''())
    ;; Potential binaries.  First, resolve files.
    (((? procedure? p))
     (match (p '() '() 'write "")
       ;; We assume:
       ;; - only "scripts" folder in files-programs
       ;; - no further dirs in scripts folder
       ;; - no files with extensions in scripts folder.
       ;; No binaries, so arguments is just `()
       (('directory "scripts" ()) ''())
       ;; Binaries!
       (('directory "scripts" ((ids files) ...))
        `(list
          #:modules `(((guix build guile-build-system)
                       #:select (target-guile-effective-version))
                      ,@%gnu-build-system-modules)
          #:phases
          (with-imported-modules `((guix build guile-build-system)
                                   ,@%gnu-build-system-modules)
            (gexp
             (modify-phases %standard-phases
               (add-after 'install 'hall-wrap-binaries
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((version (target-guile-effective-version))
                          (site-ccache (string-append "/lib/guile/" version
                                                      "/site-ccache"))
                          (site (string-append "/share/guile/site/" version))
                          (dep-path
                           (lambda (env path)
                             (list env ":" 'prefix
                                   (cons (string-append (ungexp output) path)
                                         (map (lambda (input)
                                                (string-append
                                                 (assoc-ref inputs input)
                                                 path))
                                              (list ,@guile-dep-labels))))))
                          (bin (string-append (ungexp output) "/bin/")))
                     (for-each (lambda (file)
                                 (wrap-program (string-append bin file)
                                   (dep-path "GUILE_LOAD_PATH" site)
                                   (dep-path "GUILE_LOAD_COMPILED_PATH"
                                             site-ccache)
                                   (dep-path "GUILE_EXTENSIONS_PATH" "/lib")))
                               (list ,@files))))))))))))
    (e (throw 'invalid-binaries e))))

(define (guix-package spec type)
  "Return a guix package description of the hall project specification SPEC, of
TYPE 'local, 'local-tarball', 'git or 'tarball."
  `(package
    (name ,(full-project-name spec))
    (version ,(specification-version spec))
    (source
     ,(match type
        ('local
         `(local-file (dirname (current-filename))
           #:recursive? #t #:select?
           (lambda (file stat)
             (not (any (lambda (my-string)
                         (string-contains file my-string))
                       (list ".git" ".dir-locals.el" "guix.scm"))))))
        ('local-tarball
         `(local-file ,(string-append "./" (full-project-name spec) "-"
                                      (specification-version spec)
                                      ".tar.gz")))
        ('git
         `(origin
           (method git-fetch)
           (uri (git-reference
                 (url ,(specification-home-page spec))
                 (commit "*insert git-commit-reference here*")))
           (file-name ,(string-append
                        (full-project-name spec) "-"
                        (specification-version spec)
                        "-checkout"))
           (sha256
            (base32 "*insert hash here*"))))
        ('tarball
         `(origin
           (method url-fetch)
           (uri ,(string-append (specification-home-page spec)
                                "/insert/path/to/tarball/here"))
           (sha256
            (base32 "*insert hash here*"))))))
    (build-system gnu-build-system)
    (arguments
     ,(guix-wrap-binaries spec))
    (native-inputs
     ,(if (features-nls (specification-features spec))
          `(list autoconf automake gnu-gettext pkg-config texinfo)
          `(list autoconf automake pkg-config texinfo)))
    (inputs (list guile-3.0))
    (propagated-inputs
     (list ,@(let ((packages (map dependency->package+module
                                  (dependencies->items
                                   (specification-dependencies spec)))))
               (if (use-guix-specs-for-dependencies?)
                   (map guix-package->variable packages)
                   packages))))
    (synopsis ,(specification-synopsis spec))
    (description ,(specification-description spec))
    (home-page ,(specification-home-page spec))
    (license ,(symbol-append 'license: (specification-license spec)))))

(define (hconfig-file)
  "Return a hall file procedure describing a file containing the hconfig
settings derived from the hall spec passed to it."
  (file
   "hconfig" scheme-filetype
   (λ (spec)
     (for-each (λ (n) (pretty-print n) (newline))
               `((define-module
                   (,(string->symbol (specification-name spec)) hconfig)
                   #:use-module (srfi srfi-26)
                   #:export (%version
                             %author
                             %license
                             %copyright
                             %gettext-domain
                             G_ N_
                             init-nls
                             init-locale))
                 (define %version ,(specification-version spec))
                 (define %author ,(specification-author spec))
                 (define %license ,(list 'quote (specification-license spec)))
                 (define %copyright ,(list 'quote (specification-copyright spec)))
                 ,@(if (nls-feature?)
                       `((define %gettext-domain ,(full-project-name spec))
                         (define G_ (cut gettext <> %gettext-domain))
                         (define N_ (cut ngettext <> <> <> %gettext-domain))
                         (define (init-nls)
                           "Bind this project's textdomain."
                           (bindtextdomain %gettext-domain "@localedir@"))
                         (define (init-locale)
                           "Install the current locale settings."
                           (catch 'system-error
                             (lambda _
                               (setlocale LC_ALL ""))
                             (lambda args
                               (false-if-exception (setlocale LC_ALL "en_US.utf8"))))
                           (init-nls)
                           (textdomain %gettext-domain)))
                       `((define %gettext-domain ,(full-project-name spec))
                         (define G_ identity)
                         (define N_ identity)
                         (define (init-nls) "Dummy as no NLS is used" #t)
                         (define (init-locale)
                           "Dummy as no NLS is used"
                           #t))))))
   #t))

(define* (guix-file #:optional (type 'local))
  "Return a hall file procedure with default contents for the project's
guix.scm file."
  (file
   "guix" scheme-filetype
   (lambda (spec)
     (for-each
      (lambda (n) (pretty-print n) (newline))
      (let* ((lst (list (match type
                          ('local (guix-package spec type))
                          (_ `(define-public
                                ,(string->symbol
                                  (full-project-name spec))
                                ,(guix-package spec type))))))
             (features (specification-features spec))
             (modules (if (use-guix-specs-for-dependencies?)
                          (let* ((deps (dependencies->items
                                        (specification-dependencies spec)))
                                 (packages (map dependency->package+module
                                                deps)))
                            (map (compose file-name->module-name
                                          location-file
                                          package-location)
                                 packages))
                          '())))        ;not using guix specifications
        (match type
          ('local
           (cons
            (cons
             'use-modules
             (delete-duplicates
              (sort `((gnu packages)
                      (gnu packages autotools)
                      ,@(if (features-nls features)
                            (list '(gnu packages gettext))
                            '())
                      (gnu packages guile)
                      (gnu packages guile-xyz)
                      (gnu packages pkg-config)
                      (gnu packages texinfo)
                      ,@modules
                      (guix build-system gnu)
                      (guix download)
                      (guix gexp)
                      ((guix licenses) #:prefix license:)
                      (guix packages)
                      (srfi srfi-1))
                    (lambda (x y)
                      (define (normalize s)
                        (format #f "~{~a~^ ~}" (flatten s)))
                      (string<? (normalize x) (normalize y))))))
            lst))
          (_ lst))))) #t))

(define* (brew-file)
  "Return a Brew file procedure with default contents for the project's
guix.scm file."
  (file
   "brew" ruby-filetype
   (lambda (spec)
     (let ((name (string-downcase (specification-name spec))))
       (format #t "class ~a < formula
  desc ~s
  homepage ~s
  url <<<<<INSERT URL TO TARBALL HERE>>>>>
  sha256 <<<<<INSERT SHA256 HERE>>>>>

  bottle do
    root_url ~s
    rebuild 1
    sha256 cellar: :any_skip_relocation, catalina: <<<<<INSERT SHA256 HERE>>>>>
  end

  depends_on \"autoconf\" => :build
  depends_on \"automake\" => :build
  depends_on \"pkg-config\" => :build
  depends_on \"texinfo\" => :build
  depends_on \"guile\"
  ~a

  def install
    ENV[\"GUILE_AUTO_COMPILE\"] = \"0\"

    # We need this so we can find other modules.
    ENV[\"GUILE_LOAD_PATH\"] = HOMEBREW_PREFIX/\"share/guile/site/3.0\"
    ENV[\"GUILE_LOAD_COMPILED_PATH\"] = HOMEBREW_PREFIX/\"lib/guile/3.0/site-ccache\"
    ENV[\"GUILE_SYSTEM_EXTENSIONS_PATH\"] = HOMEBREW_PREFIX/\"lib/guile/3.0/extensions\"

    system \"autoreconf\", \"-vif\"
    system \"./configure\", \"--prefix=#{prefix}\"
    system \"make\", \"install\"
  end

  def caveats
    <<~~EOS
      Remember to add the following to your .bashrc or equivalent in order to use this module:
        export GUILE_LOAD_PATH=\"#{HOMEBREW_PREFIX}/share/guile/site/3.0\"
        export GUILE_LOAD_COMPILED_PATH=\"#{HOMEBREW_PREFIX}/lib/guile/3.0/site-ccache\"
        export GUILE_SYSTEM_EXTENSIONS_PATH=\"#{HOMEBREW_PREFIX}/lib/guile/3.0/extensions\"
    EOS
  end

  test do
    ~a = testpath/~s
    ~a.write <<~~EOS
      (use-modules (~a))
    EOS

    ENV[\"GUILE_AUTO_COMPILE\"] = \"0\"
    ENV[\"GUILE_LOAD_PATH\"] = HOMEBREW_PREFIX/\"share/guile/site/3.0\"
    ENV[\"GUILE_LOAD_COMPILED_PATH\"] = HOMEBREW_PREFIX/\"lib/guile/3.0/site-ccache\"
    ENV[\"GUILE_SYSTEM_EXTENSIONS_PATH\"] = HOMEBREW_PREFIX/\"lib/guile/3.0/extensions\"

    system \"guile\", ~a
  end
end
"
               ;; intro
               (string-join (string-split (full-project-name spec) #\-))
               (specification-description spec)
               (specification-home-page spec)
               ;; bottle DSL
               (string-append
                "https://github.com/aconchillo/homebrew-guile/releases/download/"
                (full-project-name spec) "-" (specification-version spec))
               ;; dependencies generator
               (string-join
                (map (compose (cut string-append "depends_on \"" <> "\"")
                              first)
                     (cadr (specification-dependencies spec)))
                "\n  ")
               ;; Tests
               name (string-append name ".scm") name name
               (string-append name ".scm")))) #t))

(define (hall-template-file)
  "Return the hall template file procedure."
  (file
   "hall.commented" jinja2-filetype
   (λ (spec)
     (let ((name (full-project-name spec)))
       (format #t
               ";;                                                       -*- coding: utf-8 -*-
;;
{% for copyright_line in copyright_lines %}
;; {{ copyright_line }}
{% endfor %}
{% if copyright_lines and spdx_expressions %}
;;
{% endif %}
{% for expression in spdx_expressions %}
;; SPDX-License-Identifier: {{ expression }}
{% endfor %}
{% if \"GPL-3.0-or-later\" in spdx_expressions %}
;;
;; This file is part of ~a.
;;
;; ~a is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; ~a is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with ~a; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
{% endif %}

;;; Commentary:
;;
;;; Code:

" name name name name)))))

;; A lookup table of files that have templatized contents.
(define (templatize-files project-name)
  "Return a hash-table keyed on absolute filepaths containing the contents of
all default files that contain non-empty contents."
  (let ((htable (make-hash-table)))
    (for-each (lambda (entries)
                (let lp ((todo (entries '() '() 'raw+contents "")))
                  (match todo
                    (() #t)
                    ((((? string? fname) . contents) . rest)
                     (hash-set! htable fname contents)
                     (lp rest))
                    (((? list? dir) . rest)
                     (lp dir)
                     (lp rest))
                    (((? string? fname) . contents)
                     (hash-set! htable fname contents)))))
              (append (base-documentation project-name)
                      (base-infrastructure) (base-autotools-documentation)
                      (base-autotools-infrastructure)))
    htable))

;;;;; Validators

;;;; Return the to be validated value or throw an error!

(define (name project-name)
  (or (and (string? project-name) project-name)
      (quit-with-error
       "PROJECT-NAME should be a string."
       project-name)))

(define (prefix project-prefix)
  (or (and (string? project-prefix) project-prefix)
      (quit-with-error
       "PROJECT-prefix should be a string."
       project-prefix)))

(define (version project-version)
  (or (and (string? project-version) project-version)
      (quit-with-error
       "PROJECT-VERSION should be a string."
       project-version)))

(define (author project-author)
  (or (and (string? project-author) project-author)
      (quit-with-error
       "PROJECT-AUTHOR should be a string."
       project-author)))

(define (email project-email)
  (or (and (string? project-email) project-email)
      (quit-with-error
       "PROJECT-EMAIL should be a string."
       project-email)))

(define (synopsis project-synopsis)
  (or (and (string? project-synopsis) project-synopsis)
      (quit-with-error
       "PROJECT-SYNOPSIS should be a string."
       project-synopsis)))

(define (description project-description)
  (or (and (string? project-description) project-description)
      (quit-with-error
       "PROJECT-DESCRIPTION should be a string."
       project-description)))

(define (home-page project-home-page)
  (or (and (string? project-home-page) project-home-page)
      (quit-with-error
       "PROJECT-HOME-PAGE should be a string."
       project-home-page)))

(define (license-prs project-license)
  (or (and (symbol? project-license)
           (module-ref (resolve-interface '(config licenses)) project-license)
           project-license)
      (quit-with-error "PROJECT-LICENSE should be a known license."
                       project-license)))

(define (copyright project-copyrights)
  (match project-copyrights
    (((? number?) ...) project-copyrights)
    (_ (quit-with-error
        "PROJECT-COPYRIGHTs should be one or more numbers."
        project-copyrights))))

(define* (dependencies project-dependencies)
  "Validate the dependencies were given in the appropriate format."
  (guard (ex ((invalid-dependencies-field? ex)
              (quit-with-error
               "PROJECT-DEPENDENCIES should be one or more Guix style dependencies."
               (invalid-dependencies-field-sexp ex)))
             ((invalid-dependency-input? ex)
              (quit-with-error
               "PROJECT-DEPENDENCIES the following input is malformed."
               (invalid-dependency-input-sexp ex)))
             ((invalid-guix-input-specification? ex)
              (quit-with-error
               "PROJECT-DEPENDENCIES the following Guix input specification\
 did not match a package."
               (invalid-guix-input-specification-text ex))))
    (for-each dependency->package+module
              (dependencies->items project-dependencies))
    project-dependencies))

(define (skip project-skip)
  (match project-skip
    ;; FIXME: 2020-05-25: Added temporary allowance for optional skip.
    ;; This is to help migration of specs from 0.4 and earlier to 0.5.
    (#f '())
    (((? string?) ...) project-skip)
    (((? (match-lambda
           (((or 'clean 'scan) . ((? string?) ...)) #t)
           (_ #f)))
      ...)
     project-skip)
    (_ (quit-with-error
        "PROJECT-SKIP should be a list of strings."
        project-skip))))

(define (features-prs project-features)
  (match project-features
    ;; FIXME: 2022-08-13: As above, but for implementing feature support.
    (#f (features #f #f #f #f))
    ((((? symbol?) (? boolean?)) ...)
     (apply features (map (cut href project-features <>)
                          '(guix
                            use-guix-specs-for-dependencies
                            native-language-support
                            licensing))))
    (_ (quit-with-error
        (format #f
                "~a should be ~a, got ~s instead."
                "PROJECT-FEATURES" "a list of format '((feature #t|#f) ...)"
                project-features)))))

(define (all-files files) files)

;;;; Utilities

(define (flatten files)
  "Return a list of depth 1 from the possibly deep list FILES."
  (match files
    (() '())
    (((? list? first) . rest)
     (append (flatten first) (flatten rest)))
    ((first . rest)
     (cons first (flatten rest)))))

(define (merge-skip spec skip)
  "Return a new, valid, skip list by merging the skip field in SPEC and SKIP."
  (append (or (specification-skip spec) '()) skip))

(define (merge-skip-clean spec skip)
  "Return the clean portion of the skip spec derived from SPEC and SKIP."
  (append (match (specification-skip spec)
            ;; The spec's skip is empty if it's empty or #f
            ((or () #f) '())
            ;; If the spec's skip contains lists, then we have specialised skips
            (((? list? l))
             (match (href l 'clean)
               ((or () #f) '())
               (('clean . rest) rest)))
            ;; Otherwise we have general skips
            (spec-skip spec-skip))
          skip))

(define (merge-skip-scan spec skip)
  "Return the scan portion of the skip spec derived from SPEC and SKIP."
  ;; Virtually identical to above
  (append (match (specification-skip spec)
            ;; The spec's skip is empty if it's empty or #f
            ((or () #f) '())
            ;; If the spec's skip contains lists, then we have specialised skips
            (((? list? l))
             (match (href l 'scan)
               ((or () #f) '())
               (('scan . rest) rest)))
            ;; Otherwise we have general skips
            (spec-skip spec-skip))
          skip))

;;;; Hall file parser

(define (href scm key)
  "Return the value for KEY in the SXML representation of a hall specification
SCM."
  (match (assoc-ref scm key)
    ((value) value)
    ((values ...) values)
    (#f #f)))

(define (category-traverser files project-name)
  "Return a list of hall style directory or file procedures from the SXML
representation of files FILES, under PROJECT-NAME."
  (let lp ((files files)
           (accum '())
           (path '()))
    (match files
      (() (reverse accum))
      ;; recurse
      ((('directory name children) . rest)
       (lp rest
           (cons (directory name (lp children '() (cons name path))) accum)
           path))
      (((type name . args) . rest)
       (lp rest
           (cons (apply filetype-read type name
                        (file->filepath type name (reverse path))
                        (templatize-files project-name) args)
                 accum)
           path))
      (_ (throw 'hall-category-traverser "Got muddled:" files accum)))))

(define (scm->files all-files project-name)
  "Return a hall specification files section for the entire SXML representation
of the files ALL-FILES under PROJECT-NAME."
  (apply files
         (map (compose (cut category-traverser <> project-name)
                       (cute href all-files <>))
              '(libraries tests programs documentation infrastructure))))

(define (scm->features scm)
  "Like scm->specification, but only reads features.  This is useful
because some other field reader/sanitizers depend on the value of
features."
  (match scm
    ((or ('hall-description . scm) scm)
     (features-prs (href scm 'features)))
    (_
     (quit-with-error
      "It looks like your hall file has been corrupted.  You may have to
regenerate it."))))

(define* (scm->specification scm #:optional files)
  "Return a hall specification of the SXML representation of that specification
SCM."
  (match scm
    ((or ('hall-description . scm) scm)
     (apply specification
            (append (map (match-lambda
                           ((label proc)
                            (proc (href scm label))))
                         `((name ,name) (prefix ,prefix) (version ,version)
                           (author ,author) (email ,email)
                           (copyright ,copyright) (synopsis ,synopsis)
                           (description ,description) (home-page ,home-page)
                           (license ,license-prs) (dependencies ,dependencies)
                           (features ,features-prs) (skip ,skip)))
                    (list (or files (scm->files (href scm 'files)
                                                (href scm 'name)))))))
    (_
     (quit-with-error
      "It looks like your hall file has been corrupted.  You may have to
regenerate it."))))

(define (filetype-read type name fname templates . args)
  "Return a hall file procedure for the SXML represented file of language TYPE,
with name NAME, and full path name FNAME, potentially with contents found in
the hash-table keyed on FNAMEs TEMPLATES, or in ARGS."
  ;; First element in args is considered a user specified content.
  (let ((contents (if (not (null? args))
                      (first args)
                      (hash-ref templates fname "")))
        (ft (filetype-find (cut eqv? <> type))))
    (if (equal? ft symlink-filetype)
        (slink name contents)
        (file name ft contents))))

(define (file->filepath type name path)
  "Return the absolute filepath of the file NAME of language TYPE at folder
PATH."
  (context->fname path name
                  (filetype-extension (filetype-find (cut eqv? <> type)))))
