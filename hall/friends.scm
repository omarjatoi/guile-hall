;; Hall/Friends.scm --- friends implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2018-2020 Alex Sassmannshausen <alex@pompo.co>
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

(define-module (hall friends)
  #:use-module (hall common)
  #:use-module (hall config)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (guix
            gettextize
            autoreconf
            reuse license-map))

;;;; Guix

(define (guix args)
  (catch 'friends
    (lambda _
      (run "guix" args "GNU Guix"
           (list
            "Run make targets from the `build' subcommand"
            "Maintain your Native Language Support system, if enabled")))
    (lambda (key message code . rest)
      (match code
        (1
         (quit-with-error "Automake failed.

If you have just enabled NLS, you may need to run `hall dist -xf` to re-generate
your guix recipe.

Otherwise this likely means your build infrastructure is hosed. The best thing
to do is probably to re-generate it using

  $ hall build -xf

WARNING: this will delete any changes you have made to the build system
yourself!"))
        (2
         (quit-with-error "Guix was unable to run your make target.

Either it is invalid, or you have a bug in your code, or your build system
contains an error.  If it's the latter you can have hall re-generate it using

 $ hall build -xf

WARNING: this will delete any changes you have made to the build system
yourself!"))))))

(define (autoreconf args)
  (catch 'friends
    (λ _
      (run "autoreconf" args "Autoconf"
           (list
            "Automatically generate GNU build system files.")))
    (lambda (key message code . rest)
      (match code
        (1
         (quit-with-error "Autoconf exited with an error.

You are likely missing some parts of the autotools build infrastructure. Please
make sure you have automake installed."))
        (2
         (quit-with-error "Autoconf exited with an error.

You are likely missing aclocal. This can usually be found in the automake
package."))))))

(define (gettextize args)
  (catch 'friends
    (lambda _
      (run "gettextize --force" args "Gettextize"
           (list
            "Prepare your project for Native Language Support using GNU Gettext"
            "Keep your program up to date with gettext and translations")))
    (lambda (key message code . rest)
      (match code
        (1
         (quit-with-error "Gettextize exited with an error.

You are likely missing some parts of the autotools build infrastructure. Please
make sure you have automake installed."))))))

;;;; Reuse

(define (reuse . args)
  (catch 'friends
    (lambda _
      (run "reuse" (string-join args " ") "Reuse"
           (list
            "Manage copyright assignments in files"
            "Manage licensing in files"
            "Manage project LICENSES"
            "Manage file headers using templates")))
    (lambda (key message code . rest)
      (match code
        (2
         (quit-with-error "Reuse exited with an error.

Your requested hall operation may have completed, but the reuse part failed.

Please have a look at the error message above. If this looks like a bug in Hall,
please report it to us."))))))

;;;; General

(define (run cmd args name features)
  (let ((full-cmd (string-join `(,cmd ,args) " ")))
    (if (or (string=? cmd "guix")
            (not (guix-feature?)))
        (match (status:exit-val (system full-cmd))
          (127
           (format #t "It seems ~a is not installed.
~a is an optional component of Hall, so you don't have to install it, but it
provides the following additional features:
~a~%" name name (string-join (map (cut string-append "- " <>) features) "\n"))
           (exit 1))
          (0 #t)
          (n (throw 'friends (format #f "~a returned an error code: ~a. Aborting.~%"
                                     name n) n)))
        (guix (format #f "shell -Df guix.scm -- ~a" full-cmd)))))

(define license-map
  '((agpl1 . AGPL-1.0-only)
    (agpl3 . AGPL-3.0-only)
    (agpl3+ . AGPL-3.0-or-later)
    (cc0 . CC0-1.0)
    (cc-by2.0 . CC-BY-2.0)
    (cc-by3.0 . CC-BY-3.0)
    (cc-by-sa2.0 . CC-BY-SA-2.0)
    (cc-by-sa3.0 . CC-BY-SA-3.0)
    (cc-by-sa4.0 . CC-BY-SA-4.0)
    (cddl1.0 . CDDL-1.0)
    (cecill-b . CECILL-B)
    (cecill-c . CECILL-C)
    (artistic2.0 . Artistic-2.0)
    (cpl1.0 . CPL-1.0)
    (epl1.0 . EPL-1.0)
    (freebsd-doc . FreeBSD-DOC)
    (giftware . Giftware)
    (gpl1 . GPL-1.0-only)
    (gpl1+ . GPL-1.0-or-later)
    (gpl2 . GPL-2.0-only)
    (gpl3 . GPL-3.0-only)
    (gpl3+ . GPL-3.0-or-later)
    (fdl1.2+ . GFDL-1.2-or-later)
    (fdl1.1+ . GFDL-1.1-or-later)
    (ijg . IJG)
    (lgpl2.0 . LGPL-2.0)
    (lgpl2.0+ . LGPL-2.0-or-later)
    (lgpl2.1 . LGPL-2.1-only)
    (lgpl2.1+ . LGPL-2.1-or-later)
    (lgpl3 . LGPL-3.0)
    (lgpl3+ . LGPL-3.0-or-later)
    (unlicense . Unlicense)))
