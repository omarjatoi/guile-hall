;; hall/init.scm --- init implementation    -*- coding: utf-8 -*-
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

(define-module (hall init)
  #:use-module (hall builders)
  #:use-module (hall common)
  #:use-module (hall spec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (create-project create-project-here recreate-project-here))

;; Fire of side-effectful project creation
(define (create-project spec context operation)
  (let ((fname (context->fname context (full-project-name spec))))
    (if (file-exists? fname)
        (throw 'hall-create-project "PROJECT already exists: "
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
  (when (eq? 'show operation)
    (format #t "Dryrun:~%"))
  (instantiate spec context operation)
  (when (eq? 'show operation)
    (format #t "Finished dryrun.~%")))

(define (recreate-project-here spec context operation)
  (when (eq? operation 'exec)
    (for-each (lambda (n)
                (false-if-exception (delete-file n)))
              '("COPYING" "HACKING")))
  (create-project-here spec context operation))
