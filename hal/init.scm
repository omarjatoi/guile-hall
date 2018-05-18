;; hal/init.scm --- init implementation    -*- coding: utf-8 -*-
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

(define-module (hal init)
  #:use-module (hal builders)
  #:use-module (hal common)
  #:use-module (hal spec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (create-project create-project-here))

;; Fire of side-effectful project creation
(define (create-project spec context operation)
  (let ((fname (context->fname context (specification-name spec))))
    (if (file-exists? fname)
        (throw 'hal-create-project "PROJECT already exists: "
               (specification-name spec))
        (begin
          (match operation
            ('exec (mkdir fname))
            ((or 'show _) (format #t "Creating project: ~a~%" fname)))
          (instantiate spec (append context `(,(specification-name spec)))
                       operation)))))


(define (create-project-here spec context operation)
  (instantiate spec context operation))
