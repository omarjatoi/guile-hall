;; hall/build.scm --- build implementation    -*- coding: utf-8 -*-
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

(define-module (hall build)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (hall common)
  #:export (make-build-infrastructure))

(define (make-build-infrastructure spec context operation force)
  "Commandline tool for setting up the projects build system.  SPEC is a hall
specification file for the project in question.  CONTEXT is a list containing
as its first and only element the absolute filepath to the project
base-directory.  OPERATION can be 'show or 'exec."
  (when (eq? 'show operation)
    (format #t "Dryrun:~%"))
  (for-each (lambda (file) (file spec context (if (and force (eqv? operation 'exec))
                                             're-exec
                                             operation)
                            ""))
            (base-autotools))
  (when (eq? 'show operation)
    (format #t "Finished dryrun.~%")))
