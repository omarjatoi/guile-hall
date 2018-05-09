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
  #:use-module (hal spec)
  #:use-module (hal builders)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (values->specification
            instantiate

            project-root-directory? find-project-root-directory

            read-spec))

(define (values->specification nam versio autho copyrigh synopsi descriptio
                               home-pag licens dependencie
                               lib-files tst-files prog-files doc-files
                               infra-files)
  (specification
   (name nam) (version versio) (author autho) (copyright copyrigh)
   (synopsis synopsi) (description descriptio) (home-page home-pag)
   (license licens) (dependencies dependencie)
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
  `(,(directory "bin" `())))

(define (base-documentation name)
  `(,(file "README" 'txt #f "")
    ,(file "HACKING" 'txt #f "")
    ,(file "COPYING" 'txt #f "")
    ,(directory "doc"
                `(,(file name 'texi "texi" "")))))

(define (base-infrastructure)
  `(,(file "guix" 'scheme "scm" "")
    ,(file "halcyon" 'scheme "scm" #f)))

;;;;; Validators

(define (name project-name)
  (or (and (string? project-name) project-name)
      (throw 'hal-spec-name "PROJECT-NAME should be a string.")))

(define (version project-version)
  (or (and (string? project-version) project-version)
      (throw 'hal-spec-version "PROJECT-VERSION should be a string.")))

(define (author project-author)
  (or (and (string? project-author) project-author)
      (throw 'hal-spec-author "PROJECT-AUTHOR should be a string.")))

(define (synopsis project-synopsis)
  (or (and (string? project-synopsis) project-synopsis)
      (throw 'hal-spec-synopsis "PROJECT-SYNOPSIS should be a string.")))

(define (description project-description)
  (or (and (string? project-description) project-description)
      (throw 'hal-spec-description
             "PROJECT-DESCRIPTION should be a string.")))

(define (home-page project-home-page)
  (or (and (string? project-home-page) project-home-page)
      (throw 'hal-spec-home-page "PROJECT-HOME-PAGE should be a string.")))

;; FIXME: LICENSE should be a license object
(define (license project-license)
  (or (and (string? project-license) project-license)
      (throw 'hal-spec-license "PROJECT-LICENSE should be a string.")))

(define (copyright project-copyrights)
  (match project-copyrights
    (((? number?) ...) project-copyrights)
    (_ (throw 'hal-spec-copyrights
              "PROJECT-COPYRIGHTs should be one or more numbers."))))

(define (dependencies project-dependencies)
  (match project-dependencies
    ((((? string?) (? symbol?)) ...) project-dependencies)
    (_
     (throw 'hal-spec-dependencies
            "PROJECT-DEPENDENCIES should be one or more Guix style dependencies."))))

(define (all-files files) files)
