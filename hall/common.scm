;; hall/common.scm --- common implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2018,2019 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
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
  #:use-module (hall spec)
  #:use-module (hall builders)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web http)
  #:export (values->specification
            instantiate

            blacklisted? project-root-directory? find-project-root-directory

            read-spec filetype-read
            scm->files scm->specification

            guix-file

            base-autotools
            base-autotools-documentation base-autotools-infrastructure

            flatten))

(define (values->specification nam prefi versio autho copyrigh synopsi
                               descriptio home-pag licens dependencie
                               lib-files tst-files prog-files doc-files
                               infra-files)
  "Return a hal specification of a project with the arguments.  The arguments
are checked against basic validations before a specification is returned."
  (specification
   (name nam) (prefix prefi) (version versio) (author autho)
   (copyright copyrigh) (synopsis synopsi) (description descriptio)
   (home-page home-pag) (license-prs licens) (dependencies dependencie)
   (all-files
    (files (append lib-files (base-libraries nam))
           (append tst-files (base-tests))
           (append prog-files (base-programs))
           (append doc-files (base-documentation nam))
           (append infra-files (base-infrastructure))))))

(define (instantiate spec context operation)
  "Carry out the operation OPERATION, a symbol that should be 'show or 'exec,
against the projec described by the specification SPEC in the list describing
the folder location CONTEXT."
  (for-each (cute <> spec context operation "  ")
            (apply append
                   (map (cute <> (specification-files spec))
                        (list files-libraries files-tests files-programs
                              files-documentation files-infrastructure)))))

(define (project-root-directory?)
  "Return #t if we believe to be in a project root directory, a directory
containing a hall.scm file."
  (file-exists? "hall.scm"))

(define (blacklisted? path project-root skip)
  "Return #t if the absolute filepath PATH, located in the project at absolute
filepath PROJECT-ROOT is contained in the list of relative file-paths SKIP."
  ;; Currently only allow blacklisting at the top-level.
  (and (not (string=? project-root path))
       (member (string-drop path (1+ (string-length project-root)))
               (cons* ".dir-locals.el" ".gitignore" ".git" skip))))

(define (find-project-root-directory)
  "Find and return the project root directory path of the current project, and
set the working directory to it, or throw an error."
  (let ((start (getcwd)))
    (let lp ((cwd (getcwd)))
      (cond ((project-root-directory?) `(,cwd))
            ((string=? cwd "/")
             (throw 'hall-find-project-root-directory
                    "No hall.scm file found.  Search started at:" start))
            (else
             (chdir "..")
             (lp (getcwd)))))))

(define (read-spec)
  "Set the working directory to the current project's root directory & parse
the project's hall.scm file."
  (find-project-root-directory)
  (scm->specification
   (with-input-from-file "hall.scm"
     (lambda _ (read)))))

;;;; Defaults

(define (base-libraries name)
  "Return the default libraries section."
  `(,(file name 'scheme "scm" "")
    ,(directory name '())))

(define (base-tests)
  "Return the default tests section."
  `(,(directory "tests" '())))

(define (base-programs)
  "Return the default programs section."
  `(,(directory "scripts" `())))

(define (base-top-docs)
  "Return the default top level documentation section."
  `(,(file "README" 'org "org"
           (lambda (spec)
             (format #t
                     "-*- mode: org; coding: utf-8; -*-

#+TITLE: README for ~a~%~%"
                     (friendly-project-name spec))))
    ,(slink "README" "README.org")
    ,(file "HACKING" 'text #f
           (lambda (spec)
             (format #t
                     "-*- mode: org; coding: utf-8; -*-

#+TITLE: Hacking ~a

* Contributing

By far the easiest way to hack on ~a is to develop using Guix:

#+BEGIN_SRC bash
  # Obtain the source code
  cd /path/to/source-code
  guix environment -l guix.scm
  # In the new shell, run:
  hall dist --execute && autoreconf -vif && ./configure && make check
#+END_SRC

You can now hack this project's files to your heart's content, whilst
testing them from your `guix environment' shell.

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
  hall dist -x && autoreconf -vif && ./configure && make check
#+END_SRC
"
                     (specification-name spec) (specification-name spec)
                     (string-join
                      (map (match-lambda ((label . _) label))
                           ;; first element is quasiquote
                           (second (specification-dependencies spec)))
                      "\n  - " 'prefix))))
    ,(file "COPYING" 'text #f
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
              (301 (fetch (assq-ref (response-headers response) 'location)))
              (_ (fetch-license license #f))))))
      (lambda (k . args) (fetch-license license #f))))
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
    ,(file "hall" 'scheme "scm" #f)))

(define (base-autotools-documentation)
  "Return the default autotools documentation section."
  `(,(file "NEWS" 'text #f
           (lambda (spec)
             (format #t
                     "-*- mode: org; coding: utf-8; -*-

#+TITLE: ~a NEWS – history of user-visible changes
#+STARTUP: content hidestars

Copyright © ~a ~a <INSERT EMAIL HERE>

  Copying and distribution of this file, with or without modification,
  are permitted in any medium without royalty provided the copyright
  notice and this notice are preserved.

Please send ~a bug reports to INSERT EMAIL HERE.

* Publication at ~a~%"
                     (friendly-project-name spec)
                     (specification-copyright spec)
                     (specification-author spec)
                     (friendly-project-name spec)
                     (specification-version spec))))
    ,(file "AUTHORS" 'text #f
           (lambda (spec)
             (format #t
                     "Contributers to ~a ~a:

    ~a <INSERT EMAIL HERE>~%"
                     (friendly-project-name spec) (specification-version spec)
                     (specification-author spec))))
    ,(file "ChangeLog" 'text #f
           (lambda (spec)
             (format #t
                     "For a complete log, please see the Git commit log at <~a/PATH/TO/LOG>.~%"
                     (specification-home-page spec))))))


(define (base-autotools-infrastructure)
  "Return the default autotools section."
  `(,(directory "build-aux"
                `(,(file "test-driver" 'scheme "scm"
                         "
;;;; test-driver.scm - Guile test driver for Automake testsuite harness

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
")))
    ,(configure-file)
    ,(makefile-file)
    ,(file "pre-inst-env" 'shell "in"
           "
#!/bin/sh

abs_top_srcdir=\"`cd \"@abs_top_srcdir@\" > /dev/null; pwd`\"
abs_top_builddir=\"`cd \"@abs_top_builddir@\" > /dev/null; pwd`\"

GUILE_LOAD_COMPILED_PATH=\"$abs_top_builddir${GUILE_LOAD_COMPILED_PATH:+:}$GUILE_LOAD_COMPILED_PATH\"
GUILE_LOAD_PATH=\"$abs_top_builddir:$abs_top_srcdir${GUILE_LOAD_PATH:+:}:$GUILE_LOAD_PATH\"
export GUILE_LOAD_COMPILED_PATH GUILE_LOAD_PATH

PATH=\"$abs_top_builddir/scripts:$PATH\"
export PATH

exec \"$@\"
")))

(define (base-autotools)
  "Return the complete autotools section."
  `(,@(base-autotools-documentation)
    ,@(base-autotools-infrastructure)))

;;;;; Files

(define (manual-file name)
  "Return a hal file procedure with default contents for the project's
manual."
  (file name 'texinfo "texi"
        (lambda (spec)
          (display
           (string-append "
\\input texinfo
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
* " (friendly-project-name spec) ": (" (full-project-name spec) ")    " (specification-synopsis spec) "
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

(define (configure-file)
  "Return a hal file procedure with default contents for the project's
configure.ac file."
  (file "configure" 'autoconf "ac"
        (lambda (spec)
          (display
           (string-append "
dnl -*- Autoconf -*-

AC_INIT(" (full-project-name spec) ", " (specification-version spec) ")
AC_SUBST(HVERSION, \"\\\"" (specification-version spec) "\\\"\")
AC_SUBST(AUTHOR, \"\\\"" (specification-author spec) "\\\"\")
AC_SUBST(COPYRIGHT, \"'" (object->string (specification-copyright spec)) "\")
AC_SUBST(LICENSE, " (symbol->string (specification-license spec)) ")
AC_CONFIG_SRCDIR(" (let ((core-file (string-append (specification-name spec)
                                                   ".scm")))
                     (if (file-exists? core-file)
                         core-file
                         (match (find (match-lambda (('directory . rest) #t)
                                                    (_ #f))
                                      (map (cut <> '() '() 'write "")
                                           (files-libraries
                                            (specification-files spec))))
                           (('directory name children) name)))) ")
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
"
                           (string-join
                            (map (lambda (file)
                                   (let ((file (or (and=> (string-match "\\.in$" file)
                                                          (cut regexp-substitute #f <> 'pre))
                                                   file)))
                                     (string-append "AC_CONFIG_FILES([" file
                                                    "],[chmod +x " file "])")))
                                 (flatten (map (cut <> '() '() 'raw "")
                                               (files-programs
                                                (specification-files spec)))))
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

dnl Hall auto-generated guile-module dependencies
"
                           (string-join
                            (filter-map
                             (match-lambda
                               ;; Standard Guix dependency declaration
                               ((_ ('unquote _) . _) #f)
                               ;; Augmented Hall dependency declaration
                               ((_ (module ...) . _)
                                (string-append "GUILE_MODULE_REQUIRED(["
                                               (string-join
                                                (map symbol->string module)
                                                " ")
                                               "])")))
                             ;; first element is quasiquote
                             (second (specification-dependencies spec)))
                            "\n")
                           "

dnl Installation directories for .scm and .go files.
guilemoduledir=\"${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION\"
guileobjectdir=\"${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache\"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_OUTPUT
")))))

;;;; Full on cargo cult!
(define (makefile-file)
  "Return a hal file procedure with default contents for the project's
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
   "Makefile" 'automake "am"
   (lambda (spec)
     (display
      (string-append "

bin_SCRIPTS = " (string-join
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

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\\
  -e 's,[@]GUILE[@],$(GUILE),g'				\\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS = pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o \"$@\" \"$<\"

SOURCES = " (string-join
             (align
              (flatten (map (cute <> spec '() 'raw "")
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
dvi: # Don't build dvi docs

EXTRA_DIST += " (string-join
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
                  14)
                 " \\\n") " \\
              build-aux/test-driver.scm \\
              $(TESTS)

ACLOCAL_AMFLAGS = -I m4

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\\
  $(GOBJECTS)					\\
  $(TESTS:tests/%.scm=%.log)
")))))

(define-syntax timed-expression
  (lambda (x)
    "Return expression EXP so that it will be evaluated in N+1 rounds of
evaluation."
    (syntax-case x ()
      ((_ 0 exp . _) #'exp)
      ((_ n exp)
       #``(quote ,(timed-expression #,(1- (syntax->datum #'n)) exp))))))

(define (guix-wrap-binaries spec)
  "Return a guix arguments section, which, if spec has binaries, wraps each
binary in such a way that it can be run without additional dependencies being
installed in a profile."
  (define dep-labels
    (timed-expression 2 (map first
                             (second (specification-dependencies spec)))))
  (match (files-programs (specification-files spec))
    ;; No binaries, so arguments is just `()
    (() ``())
    ;; Potential binaries.  First, resolve files.
    (((? procedure? p))
     (match (p '() '() 'write "")
       ;; We assume:
       ;; - only "scripts" folder in files-programs
       ;; - no further dirs in scripts folder
       ;; - no files with extensions in scripts folder.
       ;; No binaries, so arguments is just `()
       (('directory "scripts" ()) ``())
       ;; Binaries!
       (('directory "scripts" ((ids files) ...))
        `(quasiquote
          (#:modules
           ((ice-9 match) (ice-9 ftw)
            ,@%gnu-build-system-modules)
           #:phases
           (modify-phases %standard-phases
             (add-after 'install 'hall-wrap-binaries
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((compiled-dir
                         (lambda (out version)
                           (string-append
                            out "/lib/guile/" version "/site-ccache")))
                        (uncompiled-dir
                         (lambda (out version)
                           (string-append
                            out "/share/guile/site"
                            (if (string-null? version) "" "/") version)))
                        (dep-path
                         (lambda (env modules path)
                           (list env ":" 'prefix
                                 (cons modules
                                       (map (lambda (input)
                                              (string-append
                                               (assoc-ref inputs input)
                                               path))
                                            ,,dep-labels)))))
                        (out (assoc-ref outputs "out"))
                        (bin (string-append out "/bin/"))
                        (site (uncompiled-dir out "")))
                   (match (scandir site)
                     (("." ".." version)
                      (for-each
                       (lambda (file)
                         (wrap-program (string-append bin file)
                           (dep-path "GUILE_LOAD_PATH"
                                     (uncompiled-dir out version)
                                     (uncompiled-dir "" version))
                           (dep-path "GUILE_LOAD_COMPILED_PATH"
                                     (compiled-dir out version)
                                     (compiled-dir "" version))))
                       ,,(timed-expression 2 files))
                      #t)))))))))))))

(define (guix-package spec type)
  "Return a guix package description of the hal project specification SPEC, of
TYPE 'local, 'git or 'tarball."
  `(package
     (name ,(full-project-name spec))
     (version ,(specification-version spec))
     (source
      ,(match type
         ('local
          (string-append "./" (full-project-name spec) "-"
                         (specification-version spec)
                         ".tar.gz"))
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
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("pkg-config" ,pkg-config)
        ("texinfo" ,texinfo)))
     (inputs `(("guile" ,guile-2.2)))
     (propagated-inputs
      ;; This arcane contraption generates a valid input assoc.
      ,(cons 'quasiquote
             (list (map (lambda (n)
                          (match n
                            ;; Standard Guix input declaration
                            ((_ ('unquote _) . _) n)
                            ;; Augmented Hall input declaration
                            ((label (? list?) . rest) `(,label . ,rest))))
                        ;; first element is quasiquote
                        (second (specification-dependencies spec))))))
     (synopsis ,(specification-synopsis spec))
     (description ,(specification-description spec))
     (home-page ,(specification-home-page spec))
     (license ,(symbol-append 'license: (specification-license spec)))))

(define* (guix-file #:optional (type 'local))
  "Return a hal file procedure with default contents for the project's
guix.scm file."
  (file
   "guix" 'scheme "scm"
   (lambda (spec)
     (for-each (lambda (n) (pretty-print n) (newline))
               (let ((lst (list (match type
                                  ('local (guix-package spec type))
                                  (_ `(define-public
                                        ,(string->symbol
                                          (full-project-name spec)) 
                                        ,(guix-package spec type)))))))
                 (match type
                   ('local (cons '(use-modules (guix packages)
                                               ((guix licenses) #:prefix license:)
                                               (guix download)
                                               (guix build-system gnu)
                                               (gnu packages)
                                               (gnu packages autotools)
                                               (gnu packages guile)
                                               (gnu packages guile-xyz)
                                               (gnu packages pkg-config)
                                               (gnu packages texinfo))
                                 lst))
                   (_ lst)))))))

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
      (throw 'hall-spec-name "PROJECT-NAME should be a string."
             project-name)))

(define (prefix project-prefix)
  (or (and (string? project-prefix) project-prefix)
      (throw 'hall-spec-prefix "PROJECT-prefix should be a string."
             project-prefix)))

(define (version project-version)
  (or (and (string? project-version) project-version)
      (throw 'hall-spec-version "PROJECT-VERSION should be a string."
             project-version)))

(define (author project-author)
  (or (and (string? project-author) project-author)
      (throw 'hall-spec-author "PROJECT-AUTHOR should be a string."
             project-author)))

(define (synopsis project-synopsis)
  (or (and (string? project-synopsis) project-synopsis)
      (throw 'hall-spec-synopsis "PROJECT-SYNOPSIS should be a string."
             project-synopsis)))

(define (description project-description)
  (or (and (string? project-description) project-description)
      (throw 'hall-spec-description
             "PROJECT-DESCRIPTION should be a string."
             project-description)))

(define (home-page project-home-page)
  (or (and (string? project-home-page) project-home-page)
      (throw 'hall-spec-home-page "PROJECT-HOME-PAGE should be a string."
             project-home-page)))

;; FIXME: LICENSE should be a license object
(define (license-prs project-license)
  (or (and (symbol? project-license) project-license)
      (throw 'hall-spec-license "PROJECT-LICENSE should be a symbol."
             project-license)))

(define (copyright project-copyrights)
  (match project-copyrights
    (((? number?) ...) project-copyrights)
    (_ (throw 'hall-spec-copyrights
              "PROJECT-COPYRIGHTs should be one or more numbers."
              project-copyrights))))

(define (dependencies project-dependencies)
  (match project-dependencies
    ((or ('quasiquote ())
         ('quasiquote (((? string?) ('unquote (? symbol?))) ...)))
     project-dependencies)
    (_
     (throw 'hall-spec-dependencies
            "PROJECT-DEPENDENCIES should be one or more Guix style dependencies."
            project-dependencies))))

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

;;;; Hall file parser

(define (href scm key)
  "Return the value for KEY in the SXML representation of a hal specification
SCM."
  (match (assoc-ref scm key)
    ((value) value)
    ((values ...) values)
    (#f (throw 'hall-scm->specification "Missing expected hall key:" key))))

(define (category-traverser files project-name)
  "Return a list of hal style directory or file procedures from the SXML
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
  "Return a hal specification files section for the entire SXML representation
of the files ALL-FILES under PROJECT-NAME."
  (apply files
         (map (compose (cut category-traverser <> project-name)
                       (cute href all-files <>))
              '(libraries tests programs documentation infrastructure))))

(define (scm->specification scm)
  "Return a hal specification of the SXML representation of that specification
SCM."
  (match scm
    (('hall-description . scm)
     (apply specification
            (append (map (cute href scm <>)
                         '(name prefix version author copyright synopsis
                                description home-page license dependencies))
                    (list (scm->files (href scm 'files) (href scm 'name))))))
    (_ (throw 'hall-scm->specification "Invalid hall data:" scm))))

(define (filetype-read type name fname templates . args)
  "Return a hal file procedure for the SXML represented file of language TYPE,
with name NAME, and full path name FNAME, potentially with contents found in
the hash-table keyed on FNAMEs TEMPLATES, or in ARGS."
  ;; First element in args is considered a user specified content.
  (let ((contents (or (and (not (null? args)) (first args))
                      (hash-ref templates fname ""))))
    (match type
      ('symlink (slink name contents))
      (_
       (apply file name
              (match type
                ('scheme-file `(scheme "scm" ,contents))
                ('text-file `(text #f ,contents))
                ('info-file `(info "info" ,contents))
                ('texi-file `(texinfo "texi" ,contents))
                ('shell-file `(shell "sh" ,contents))
                ('autoconf-file `(autoconf "ac" ,contents))
                ('automake-file `(automake "am" ,contents))
                ('in-file `(in "in" ,contents))
                ('m4-file `(m4 "m4" ,contents))
                ('compiled-scheme-file `(go "go" ,contents))
                ('org-file `(org "org" ,contents))
                (_ (throw 'hall-filetype-read
                          "Unknown filetype" type name args))))))))

(define (file->filepath type name path)
  "Return the absolute filepath of the file NAME of language TYPE at folder
PATH."
  (context->fname path name (match type
                              ('scheme-file "scm")
                              ('text-file #f)
                              ('info-file "info")
                              ('texi-file "texi")
                              ('shell-file "sh")
                              ('autoconf-file "ac")
                              ('automake-file "am")
                              ('in-file "in")
                              ('m4-file "m4")
                              ('compiled-scheme-file "go")
                              ('org-file "org")
                              ('symlink "")
                              (_ (throw 'file->filepath "Unknown extension"
                                        type name)))))
