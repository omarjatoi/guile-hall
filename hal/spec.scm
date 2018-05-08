;; hal/spec.scm --- spec implementation    -*- coding: utf-8 -*-
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

(define-module (hal spec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (<specification>
            specification specification?
            specification-name specification-version specification-author
            specification-copyright specification-synopsis
            specification-description specification-home-page
            specification-license specification-dependencies
            specification-files

            <files>
            files files?
            files-libraries files-tests files-programs files-documentation
            files-infrastructure

            specification->metadata specification->scm))

;;;; Spec Definition

(define-record-type <files>
  (files libraries tests programs documentation infrastructure)
  files?
  (libraries files-libraries)
  (tests files-tests)
  (programs files-programs)
  (documentation files-documentation)
  (infrastructure files-infrastructure))

(define-record-type <specification>
  (specification name version author copyright synopsis description home-page
                 license dependencies files)
  specification?
  (name specification-name)
  (version specification-version)
  (author specification-author)
  (copyright specification-copyright)
  (synopsis specification-synopsis)
  (description specification-description)
  (home-page specification-home-page)
  (license specification-license)
  (dependencies specification-dependencies)
  (files specification-files))

(set-record-type-printer!
 <specification>
 (lambda (spec port)
   (format port "#<<specification> name: ~s version: ~s author: ~s>"
           (specification-name spec) (specification-version spec)
           (specification-author spec))))

;;;; Specification->metadata

(define (specification->metadata spec)
  `((name ,(specification-name spec))
    (version ,(specification-version spec))
    (author ,(specification-author spec))
    (copyright ,(specification-copyright spec))
    (synopsis ,(specification-synopsis spec))
    (description ,(specification-description spec))
    (home-page ,(specification-home-page spec))
    (license ,(specification-license spec))
    (dependencies ,(specification-dependencies spec))))

(define (specification->files spec)
  (let ((spec-files (specification-files spec)))
    (define (proc xsr)
      (map (cut <> spec '() 'write "") (xsr spec-files)))
    `(files
      (libraries ,(proc files-libraries))
      (tests ,(proc files-tests))
      (programs ,(proc files-programs))
      (documentation ,(proc files-documentation))
      (infrastructure ,(proc files-infrastructure)))))

(define (specification->scm spec)
  `(halcyon
    ,@(specification->metadata spec)
    ,(specification->files spec)))
