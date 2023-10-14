;;                                                       -*- coding: utf-8 -*-
;;
;; SPDX-FileCopyrightText: 2022 Alex Sassmannshausen <alex@komputilo.eu>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is part of guile-hall.
;;
;; guile-hall is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; guile-hall is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
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

(define-module (hall config)
  #:use-module (config api)
  #:use-module (config parser sexp)
  #:use-module (hall hconfig)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:re-export (G_ N_)
  #:export (guix-feature?
            nls-feature?
            licensing-feature?

            project-root-directory?
            quit-with-error
            find-project-root-directory find-project-root-directory*
            expand-filename part-of-project?

            %configuration))

(define guix-feature? (make-parameter #f (lambda (v) (and (boolean? v) v))))

(define nls-feature? (make-parameter #f (lambda (v) (and (boolean? v) v))))

(define licensing-feature? (make-parameter #f (lambda (v) (and (boolean? v) v))))

(define (quit-with-error msg . args)
  (apply format (current-error-port) (string-append msg "~%") args)
  (exit 1))

(define (project-root-directory?)
  "Return #t if we believe to be in a project root directory, a directory
containing a hall.scm file."
  (file-exists? "hall.scm"))

(define (find-project-root-directory*)
  "Return the current project's root as a string.

Find and return the project root directory path of the current project, as
a string, and set the working directory to it, or throw an error."
  (let ((start (getcwd)))
    (let lp ((cwd (getcwd)))
      (cond ((project-root-directory?) cwd)
            ((string=? cwd "/")
             (quit-with-error
              "We were unable to locate your hall.scm file.  We started our
search at ~a.

Are you sure this is a hall project?

Perhaps you want to create a new hall project using `hall init'?" start))
            (else
             (chdir "..")
             (lp (getcwd)))))))

(define (find-project-root-directory)
  "Find and return the project root directory path of the current project, and
set the working directory to it, or throw an error."
  `(,(find-project-root-directory*)))

(define (expand-filename filename)
  ;; canonicalize-path??
  (string-join
   (filter (negate string-null?)
           (string-split
            ;; Expand our filename
            (if (absolute-file-name? filename)
                filename
                (string-append (getcwd) file-name-separator-string
                               filename))
            (first (string->list file-name-separator-string))))
   file-name-separator-string 'prefix))

(define (part-of-project? filename)
  (regexp-match? (string-match (string-append "^" (find-project-root-directory*)
                                              ".+")
                               filename)))

(define %configuration
  (configuration
   (name 'hall)
   (version %version)
   (author %author)
   (copyright %copyright)
   (license %license)
   (synopsis "Guile project manager")
   (description
    (format #f "~a~%~a"
            (G_ "Hall is a command-line application and a set of Guile libraries
that allow you to quickly create and publish Guile projects.  It allows you to
transparently support the GNU build system, manage a project hierarchy &
provides tight coupling to Guix.")
            (G_ "Use the subcommands to manage your project, or pass the
'--help' flag to any of them to get more information.")))
   (keywords
    (list
     (switch (name 'execute) (character #\x)
             (default #f) (test boolean?)
             (synopsis "Carry out operations, instead of displaying them."))
     (switch (name 'force)
             (default #f) (test boolean?)
             (synopsis "Re-generate transient files.")
             (description "Delete and re-create transient files. The files
considered transient depend on the sub-command that this switch is invoked on.
As a guide, transient files are those normally fully managed by hall (e.g.
guix.scm, configure.ac and makefile.am).  If you customise these, then this
switch is not for you."))))
   (subcommands
    (list
     (configuration
      (name 'clean)
      (synopsis (G_ "Clean your project."))
      (description "Generate a report of which files will be kept and which
files deleted in the cleaning process.  You can specify that you want to keep
specific files using the '--skip' argument.

Once you are happy with the result, pass the '--execute' flag to carry out the
cleaning process.")
      (wanted '((keywords execute)))
      (keywords
       (list
        (switch
         (name 'skip) (default '())
         (test (match-lambda (((? string?) ...) #t) (_ #f)))
         (handler (cut string-split <> #\,))
         (synopsis "CSV list regexp patterns to indicate files to skip.")
         (example "scripts/foo,AUTHORS,^.*rgp$")))))
     (configuration
      (name 'build-system) (alias 'build)
      (synopsis "Prepare your project for a build system.")
      (description "Generate a report of which files will be generated to
prepare the build-system for this project.

Once you are happy with the result, pass the '--execute' flag to finally
generate build-system files, and the '--force' switch to re-generate hall
managed build-system files (for the gnu build system, these are the test-driver,
the pre-inst-env helper, configure.ac and Makefile.am).  In addition, '--force'
will also trigger the full regeneration of autotools supplied files.")
      (wanted '((keywords execute force)))
      (arguments
       (list
        (argument (name 'target) (default "")
                  (test string?)
                  (synopsis "make target to run against generated build files")
                  (example "check|distcheck|dist")))))
     (configuration
      (name 'distribution-system)
      (alias 'dist)
      (description "Subcommand to Hall's support for different distribution
system.  The default distribution system we encourage, and are coupled to, is
Guix.  But more will be supported over time.

The default guix file that will be generated is one that can be used in the
project's repository, for use with Guix's 'shell' subcommand.  You can
also generate a tarball or git repository based package which can be used with
the 'guix package' subcommand, by using the '--type' argument.

Once you are happy with the result, pass the '--execute' flag to generate the
guix package file.  You can pass the '--force' switch to regenerate the
distribution file.")
      (synopsis "Manage your project's distribution files.")
      (wanted '((keywords execute force)))
      (keywords
       (list
        (switch (name 'type) (default 'local)
                (test symbol?) (handler string->symbol)
                (synopsis "Guix recipe type to generate (defaults to local)")
                (example "local|git|tarball|local-tarball"))))
      (arguments
       (list
        (argument (name 'system) (default "guix")
                  (test (λ (t) (member t '("guix" "brew"))))
                  (synopsis "The system to use")
                  (example "guix|brew")))))
     (configuration
      (name 'initiate) (alias 'init)
      (synopsis "Create or migrate a new project.")
      (description "Generate a report of which files will be generated to
create a new project.  You can use a number of arguments (see above) to
provide additional metadata — but the project name is mandatory.

Defaults for the author, prefix, website & license arguments can be set in the
~/.hall configuration file.

'--convert' can be used when you have an existing project that you want to
start using hall for.

Once you are happy with the result, pass the '--execute' flag to finally
generate the new project.")
      (wanted '((keywords execute)))
      (arguments
       (list
        (argument (name 'name) (test (negate string-null?))
                  (synopsis "Name for the project.")
                  (example "hello-world"))))
      (keywords
       (list
        (switch (name 'convert) (default #f) (test boolean?)
                (synopsis "Convert this project to a Hall project."))
        (setting (name 'author) (default "")
                 (synopsis "Author of the project.")
                 (example "\"Alyssa P. Hacker\""))
        (setting (name 'email) (default "")
                 (synopsis "Email of the author.")
                 (example "\"alyssa@example.org\""))
        (setting (name 'prefix) (default "")
                 (synopsis "Prefix of the project.")
                 (example "guile"))
        (setting (name 'website) (default "")
                 (synopsis "Home-page for the project.")
                 (example "\"https://website.mine\""))
        (setting (name 'license) (default 'gpl3+)
                 (handler string->symbol) (test symbol?)
                 (example "gpl3+")
                 (synopsis "License of your project (defaults to gpl3+)"))))
      (subcommands
       (list
        (configuration
         (name 'refresh)
         (wanted '((keywords execute)))
         (synopsis "Regenerate HACKING & COPYING files.")))))
     (configuration
      (name 'scan)
      (synopsis "Refresh your project's Hall file.")
      (description "Show a new hall.scm file that would be generated by this
command.  You can specify that you want scan to ignore specific files using
the '--skip' argument.

Once you are happy with the result, pass the '--execute' flag to actually
generate the new hall.scm file.  You will want to delete the old hall.scm file
first.")
      (wanted '((keywords execute)))
      (keywords
       (list
        (switch
         (name 'skip) (default '())
         (test (match-lambda (((? string?) ...) #t) (_ #f)))
         (handler (cut string-split <> #\,))
         (synopsis "CSV list regexp patterns to indicate files to skip.")
         (example "scripts/foo,AUTHORS,^.*rgp$")))))
     (configuration
      (name 'add)
      (synopsis "Add files or directories to your project.")
      (description "Add files or directories to your Hall file. Create them
first if they don't exist yet, from an appropriate template if
available. Specifically, if you have the reuse feature enabled, include the
appropriate licensing and copyright headers and insert boilerplate licensing
text if desired. By default this uses a template located at
/.reuse/templates/hall.commented.jinja.")
      (wanted '((keywords execute)))
      (keywords
       (list
        (setting
         (name 'template) (default "hall.commented.jinja")
         (handler (match-lambda ("" #f) (n n)))
         (test (match-lambda ((? string?) #t) (#f #t)))
         (synopsis "The name of the reuse template to use, if desired.")
         (example "\"\"|hall.commented.jinja2"))))
      (arguments
       (list
        (argument
         (name 'filename) (default #f)
         (handler expand-filename)
         (test (match-lambda ((or #f (? part-of-project?)) #t) (_ #f)))
         (synopsis "Add a single file to your hall.scm file.")
         (example "tests/frobnigator.scm"))
        (argument
         (name 'section) (default 'libraries)
         (test (match-lambda
                 ((or 'programs 'tests 'documentation 'infrastructure
                      'libraries) #t)
                 (_ #f)))
         (handler string->symbol)
         (synopsis "The section (default: libraries) to add the file to.")
         (example "programs|tests|documentation|infrastructure|libraries")))))
     (configuration
      (name 'publish) (alias 'pub)
      (synopsis "Publish your project using your build / distribution systems.")
      (description "Use your chosen build and distribution systems to publish
your project. If you use guix as your distribution system then you can use this
command to comprehensively test your project automatic publication workflow, as
well as to generate and submit a guix recipe to the guix project. If you use the
GNU build system this subcommand also allows you to generate your release
tarball, to tag your repository with a new version tag and to upload your
tarball using ssh.")
      (wanted '((keywords execute))))))
   (directory (in-home ".hall/"))
   (parser simple-sexp-parser)))
