;; hal/common.scm --- common implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2018 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;;
;; This file is part of guile-hal.
;;
;; guile-hal is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; guile-hal is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with guile-hal; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;;; Code:

(define-module (hal common)
  #:use-module (guix licenses)
  #:use-module (hal spec)
  #:use-module (hal builders)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (values->specification
            instantiate

            project-root-directory? find-project-root-directory

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
  (specification
   (name nam) (prefix prefi) (version versio) (author autho)
   (copyright copyrigh) (synopsis synopsi) (description descriptio)
   (home-page home-pag) (license licens) (dependencies dependencie)
   (all-files
    (files (append lib-files (base-libraries nam))
           (append tst-files (base-tests))
           (append prog-files (base-programs))
           (append doc-files (base-documentation nam))
           (append infra-files (base-infrastructure))))))

(define (instantiate spec context operation)
  (for-each (cute <> spec context operation "  ")
            (apply append
                   (map (cute <> (specification-files spec))
                        (list files-libraries files-tests files-programs
                              files-documentation files-infrastructure)))))

(define (project-root-directory?)
  (file-exists? "halcyon.scm"))

(define (find-project-root-directory)
  (let ((start (getcwd)))
    (let lp ((cwd (getcwd)))
      (cond ((project-root-directory?) `(,cwd))
            ((string=? cwd "/")
             (throw 'hal-find-project-root-directory
                    "No halcyon.scm file found.  Search started at:" start))
            (else
             (chdir "..")
             (lp (getcwd)))))))

(define (read-spec)
  (find-project-root-directory)
  (scm->specification
   (with-input-from-file "halcyon.scm"
     (lambda _ (read)))))

;;;; Defaults

(define (base-libraries name)
  `(,(file name 'scheme "scm" "")
    ,(directory name '())))

(define (base-tests)
  `(,(directory "tests" '())))

(define (base-programs)
  `(,(directory "scripts" `())))

(define (base-top-docs)
  `(,(file "README" 'text #f "")
    ,(file "HACKING" 'text #f
           ;; This will generate the basic HACKING file when the new project
           ;; is created.  Once the dependencies have been updated in the
           ;; halcyon file the HACKING (and the guix.scm) file will need to be
           ;; regenerated.
           (lambda (spec)
             (format #t
                     "-*- mode: org; coding: utf-8; -*-

#+TITLE: Hacking ~a

* Contributing

By far the easiest way to hack on ~a is to develop using Guix:

#+BEGIN_SRC bash
  # Obtain the source code
  cd /path/to/source-code
  guix environment --pure --container -l guix.scm
  # In the new shell, run:
  hal dist && autoreconf -vif && ./configure && make check
#+END_SRC

You can now hack the Config files to your heart's content, whilst testing them
from your `guix environment' shell.

** Manual Installation

If you do not yet use  Guix, you will have to install this project's
dependencies manually:
  - autoconf
  - automake
  - pkg-config
  - texinfo
  - guile-hal~a

Once those dependencies are installed you can run:

#+BEGIN_SRC bash
  hal dist && autoreconf -vif && ./configure && make check
#+END_SRC
"
                     (specification-name spec) (specification-name spec)
                     (string-join
                      (map (match-lambda ((label var) label))
                           (second (specification-dependencies spec)))
                      "\n  - " 'prefix))))
    ,(file "COPYING" 'text #f
           (lambda (spec)
             (match (specification-license spec)
               ((? defined? n)
                (let ((license (eval (specification-license spec)
                                     (interaction-environment))))
                  (format #t "This project's license is ~a.~%
You can read the full license at ~a.~%"
                          (license-name license)
                          (license-uri license))))
               (sym (format #t "This project's license is ~a.~%" sym)))))))

(define (base-documentation name)
  `(,@(base-top-docs)
    ,(directory "doc"
                `(,(file name 'texinfo "texi" "")))))

(define (base-infrastructure)
  `(,(guix-file)
    ,(file "halcyon" 'scheme "scm" #f)))

(define (base-autotools-documentation)
  `(,(file "NEWS" 'text #f "")
    ,(file "AUTHORS" 'text #f "")
    ,(file "ChangeLog" 'text #f "")))

(define (base-autotools-infrastructure)
  `(,(configure-file)
    ,(makefile-file)
    ,(file "test-env" 'shell "in"
           "
#!/bin/sh

\"@abs_top_builddir@/pre-inst-env\" \"$@\"

exit $?
")
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
  `(,@(base-autotools-documentation)
    ,@(base-autotools-infrastructure)))

;;;;; Files

(define (configure-file)
  (file "configure" 'autoconf "ac"
        (lambda (spec)
          (display
           (string-append "
dnl -*- Autoconf -*-

AC_INIT(" (full-project-name spec) ", " (specification-version spec) ")
AC_CONFIG_SRCDIR(" (match (find (match-lambda (('directory . rest) #t) (_ #f))
                                (map (cut <> '() '() 'write "")
                                     (files-libraries
                                      (specification-files spec))))
                     (('directory name children) name)) ")
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([test-env], [chmod +x test-env])
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
GUILE_PKG([2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR
if test \"x$GUILD\" = \"x\"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

dnl Installation directories for .scm and .go files.
guilemoduledir=\"${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION\"
guileobjectdir=\"${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache\"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_OUTPUT
")))))

;;;; Full on cargo cult!
(define (makefile-file)
  (file
   "Makefile" 'automake "am"
   (lambda (spec)
     (display
      (string-append "

bin_SCRIPTS = " (string-join
                 (map (lambda (file)
                        (or (and=> (string-match "\\.in$" file)
                                   (cut regexp-substitute #f <> 'pre))
                            file))
                      (flatten (map (cute <> spec '() 'raw "")
                                    (files-programs (specification-files spec)))))
                 " \\\n") "

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS =				\
  pre-inst-env					\
  test-env

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
             (flatten (map (cute <> spec '() 'raw "")
                           (files-libraries (specification-files spec))))
             " \\\n") "

TESTS = " (string-join
           (flatten (map (cute <> spec '() 'raw "")
                         (files-tests (specification-files spec))))
           " \\\n") "

TEST_EXTENSIONS = .scm
AM_TESTS_ENVIRONMENT = abs_top_srcdir=\"$(abs_top_srcdir)\"
SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L \"$(top_srcdir)\"

info_TEXINFOS = " (string-join
                   (filter (cut string-match ".*\\.texi$" <>)
                           (flatten
                            (map (cute <> spec '() 'raw "")
                                 (files-documentation
                                  (specification-files spec)))))
                   " \\\n") "
dvi: # Don't build dvi docs

EXTRA_DIST += " (string-join
                 (filter (negate (cut string-match ".*\\.texi$" <>))
                         (flatten
                          (map (cute <> spec '() 'raw "")
                               (files-documentation
                                (specification-files spec)))))
                 " \\\n") " \\
  # pre-inst-env.in				\\
  # test-env.in					\\
  $(TESTS)

ACLOCAL_AMFLAGS = -I m4

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\\
  $(GOBJECTS)					\\
  $(TESTS:tests/%.scm=%.log)
")))))

(define (guix-package spec type)
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
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("pkg-config" ,pkg-config)
        ("texinfo" ,texinfo)))
     (inputs `(("guile" ,guile-2.2)))
     (propagated-inputs ,(specification-dependencies spec))
     (synopsis ,(specification-synopsis spec))
     (description ,(specification-description spec))
     (home-page ,(specification-home-page spec))
     (license ,(specification-license spec))))

(define* (guix-file #:optional (type 'local))
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
                                               (guix licenses)
                                               (guix download)
                                               (guix build-system gnu)
                                               (gnu packages)
                                               (gnu packages autotools)
                                               (gnu packages guile)
                                               (gnu packages pkg-config)
                                               (gnu packages texinfo))
                                 lst))
                   (_ lst)))))))

;; A lookup table of files that have templatized contents.
(define %file-templates
  (let ((htable (make-hash-table)))
    (for-each (lambda (file)
                (hash-set! htable (file '() '() 'write "")
                           (file '() '() 'contents "")))
              (append (base-top-docs) (base-infrastructure)
                      (base-autotools-documentation)
                      (base-autotools-infrastructure)))
    htable))

;;;;; Validators

(define (name project-name)
  (or (and (string? project-name) project-name)
      (throw 'hal-spec-name "PROJECT-NAME should be a string."
             project-name)))

(define (prefix project-prefix)
  (or (and (string? project-prefix) project-prefix)
      (throw 'hal-spec-prefix "PROJECT-prefix should be a string."
             project-prefix)))

(define (version project-version)
  (or (and (string? project-version) project-version)
      (throw 'hal-spec-version "PROJECT-VERSION should be a string."
             project-version)))

(define (author project-author)
  (or (and (string? project-author) project-author)
      (throw 'hal-spec-author "PROJECT-AUTHOR should be a string."
             project-author)))

(define (synopsis project-synopsis)
  (or (and (string? project-synopsis) project-synopsis)
      (throw 'hal-spec-synopsis "PROJECT-SYNOPSIS should be a string."
             project-synopsis)))

(define (description project-description)
  (or (and (string? project-description) project-description)
      (throw 'hal-spec-description
             "PROJECT-DESCRIPTION should be a string."
             project-description)))

(define (home-page project-home-page)
  (or (and (string? project-home-page) project-home-page)
      (throw 'hal-spec-home-page "PROJECT-HOME-PAGE should be a string."
             project-home-page)))

;; FIXME: LICENSE should be a license object
(define (license project-license)
  (or (and (symbol? project-license) project-license)
      (throw 'hal-spec-license "PROJECT-LICENSE should be a string."
             project-license)))

(define (copyright project-copyrights)
  (match project-copyrights
    (((? number?) ...) project-copyrights)
    (_ (throw 'hal-spec-copyrights
              "PROJECT-COPYRIGHTs should be one or more numbers."
              project-copyrights))))

(define (dependencies project-dependencies)
  (match project-dependencies
    ((or ('quasiquote ())
         ('quasiquote (((? string?) ('unquote (? symbol?))) ...)))
     project-dependencies)
    (_
     (throw 'hal-spec-dependencies
            "PROJECT-DEPENDENCIES should be one or more Guix style dependencies."
            project-dependencies))))

(define (all-files files) files)

;;;; Utilities

(define (flatten files)
  (match files
    (() '())
    (((? list? first) . rest)
     (append (flatten first) (flatten rest)))
    ((first . rest)
     (cons first (flatten rest)))))

;;;; Halcyon file parser

(define (href scm key)
  (match (assoc-ref scm key)
    ((value) value)
    ((values ...) values)
    (#f (throw 'hal-scm->specification "Missing expected halcyon key:" key))))

(define (category-traverser files)
  (let lp ((files files)
           (accum '()))
    (match files
      (() (reverse accum))
      ;; recurse
      ((('directory name children) . rest)
       (lp rest
           (cons (directory name (lp children '())) accum)))
      (((type name . args) . rest)
       (lp rest
           (cons (apply filetype-read type name args) accum)))
      (_ (throw 'hal-category-traverser "Got muddled:" files accum)))))

(define (scm->files all-files)
  (apply files
         (map (compose category-traverser (cute href all-files <>))
              '(libraries tests programs documentation infrastructure))))

(define (scm->specification scm)
  (match scm
    (('halcyon . scm)
     (apply specification
            (append (map (cute href scm <>)
                         '(name prefix version author copyright synopsis
                                description home-page license dependencies))
                    (list (scm->files (href scm 'files))))))
    (_ (throw 'hal-scm->specification "Invalid halcyon data:" scm))))

(define (filetype-read type name . args)
  ;; First element in args is considered a user specified content.
  (let ((contents (or (and (not (null? args)) (first args))
                      (hash-ref %file-templates `(,type ,name) ""))))
    (apply file name
           (match type
             ('scheme-file `(scheme "scm" ,contents))
             ('text-file `(text #f ,contents))
             ('texi-file `(texinfo "texi" ,contents))
             ('shell-file `(shell "sh" ,contents))
             ('autoconf-file `(autoconf "ac" ,contents))
             ('automake-file `(automake "am" ,contents))
             ('in-file `(in "in" ,contents))
             (_ (throw 'hal-filetype-read
                       "Unknown filetype" type name args))))))
