;; hall/init.scm --- init implementation    -*- coding: utf-8 -*-
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

(define-module (hall init)
  #:use-module (hall builders)
  #:use-module (hall common)
  #:use-module (hall spec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (create-project create-project-here recreate-project-here))

;; Fire of side-effectful project creation
(define (create-project spec context operation)
  "Commandline tool for creating a new project.  SPEC is a hall specification
file for the project in question.  CONTEXT is a list containing as its first
and only element the absolute filepath to the project base-directory.
OPERATION can be 'show or 'exec."
  (let ((fname (context->fname context (full-project-name spec))))
    (if (file-exists? fname)
        (quit-with-error
         "The project you are trying to create (~a) already exists!"
         (full-project-name spec))
        (begin
          (when (eq? 'show operation)
            (format #t "Dryrun:~%"))
          (format #t "Creating project: ~a~%" fname)
          (when (eq? 'exec operation)
            (mkdir fname))
          (instantiate spec (append context `(,(full-project-name spec)))
                       operation)
          (when (eq? 'show operation)
            (format #t "Finished dryrun.~%"))))))


(define (create-project-here spec context operation)
  "Commandline tool for converting a project to use hall.  SPEC is a hall
specification file for the project in question.  CONTEXT is a list containing
as its first and only element the absolute filepath to the project
base-directory.  OPERATION can be 'show or 'exec."
  (when (eq? 'show operation)
    (format #t "Dryrun:~%"))
  (instantiate spec context operation)
  (when (eq? 'show operation)
    (format #t "Finished dryrun.~%")))

(define (recreate-project-here spec context operation)
  "Commandline tool for refreshing a project.  SPEC is a hall specification
file for the project in question.  CONTEXT is a list containing as its first
and only element the absolute filepath to the project base-directory.
OPERATION can be 'show or 'exec."
  (when (eq? operation 'exec)
    (for-each (lambda (n)
                (false-if-exception (delete-file n)))
              `("COPYING" "HACKING"
                ,(string-join `(,(specification-name spec) "hconfig.scm")
                              file-name-separator-string))))
  (create-project-here spec context operation))
