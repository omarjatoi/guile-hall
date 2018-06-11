;; hall/common.scm --- common implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2018 Alex Sassmannshausen <alex@pompo.co>
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
  #:use-module (guix licenses)
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
   (home-page home-pag) (license licens) (dependencies dependencie)
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
  `(,(file "README" 'text #f "")
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
                      (map (match-lambda ((label var) label))
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
    (catch 'getaddrinfo-error
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
  `(,(file "NEWS" 'text #f "")
    ,(file "AUTHORS" 'text #f "")
    ,(file "ChangeLog" 'text #f "")))

(define (base-autotools-infrastructure)
  "Return the default autotools section."
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
  "Return a hal file procedure with default contents for the project's
Makefile.am file."
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
      ,(match (files-programs (specification-files spec))
         (() ``())
         (((? procedure? p))
          (match (p '() '() 'write "")
            (('directory "scripts" ()) ``())
            ;; We assume:
            ;; - only "scripts" folder in files-programs
            ;; - no further dirs in scripts folder
            ;; - no files with extensions in scripts folder.
            (('directory "scripts" ((ids files) ...))
             ``(#:modules ((ice-9 match) (ice-9 ftw)
                           ,@%gnu-build-system-modules)
                #:phases (modify-phases %standard-phases
                           (add-after 'install 'hall-wrap-binaries
                             (lambda* (#:key outputs #:allow-other-keys)
                               (let* ((out  (assoc-ref outputs "out"))
                                      (bin  (string-append out "/bin/"))
                                      (site (string-append
                                             out "/share/guile/site")))
                                 (match (scandir site)
                                   (("." ".." version)
                                    (let ((modules (string-append site "/" version))
                                          (compiled-modules (string-append
                                                             out "/lib/guile/" version
                                                             "/site-ccache")))
                                      (for-each (lambda (file)
                                                  (wrap-program (string-append bin file)
                                                    `("GUILE_LOAD_PATH" ":" prefix
                                                      (,modules))
                                                    `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                                                      (,compiled-modules))))
                                                ,,(cons* 'list ''list files))
                                      #t)))))))))))))
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
(define (license project-license)
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
             ('compiled-scheme-file `(go "go" ,contents))
             (_ (throw 'hall-filetype-read
                       "Unknown filetype" type name args))))))

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
                              ('compiled-scheme-file "go")
                              (_ (throw 'file->filepath "Unknown extension"
                                        type name)))))
