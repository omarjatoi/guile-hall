;; tests/build.scm --- build implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2022 Alex Sassmannshausen <alex@komputilo.eu>
;;
;; Author: Alex Sassmannshausen <alex@komputilo.eu>
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
;; Source-file: hall/build.scm
;;
;;; Code:

(define-module (tests build)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (hall build))

(set! *random-state* (random-state-from-platform))
(define %build-dir #f)
(define (hall args)
  "Run the hall command as present in the build environment, not on the system."
  (string-append %build-dir "/scripts/hall " args))

(define-syntax directory-excursion
  (syntax-rules ()
    [(_ new-directory body body* ...)
     (let ([old-directory (getcwd)])
       (when (not %build-dir)
         (set! %build-dir (getcwd)))
       (dynamic-wind
         (lambda () (chdir new-directory))
         (lambda () body body* ...)
         (lambda () (chdir old-directory))))]))

(define (shell cmd)
  ;; We need to make sure that our HALL commands are invoked without loading
  ;; user hal configuration files at $HOME
  (let ((home (getenv "HOME")))
    (setenv "HOME" "/tmp/")
    (test-eq cmd 0 (status:exit-val (system cmd)))
    (setenv "HOME" home)))

(test-begin "Build")

;;; End to end testing
(test-begin "e2e")

;;; TODO: Fix leak of temporary directory
(define tmp
  ;; mkdtemp is not avaiable in Guile 3.0.2 but it's in 3.0.7
  ;;(mkdtemp "hall-tests-scan.XXXXXX")
  (let ([name (format #f  "hall-tests-build.~a" (random 4096))])
    (mkdir name)
    name))

(directory-excursion
 tmp
 (shell (hall "init -p '' foo -x"))
 (directory-excursion
  "foo"
  (shell "cat guix.scm")
  (shell (hall "dist -xf"))
  (shell "cat guix.scm")
  (shell (hall "build -xf"))))

(shell (string-append "rm -rf " tmp))

(test-end "e2e")

(test-end "Build")
