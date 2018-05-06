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
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export ())

;;;; Spec Definition

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

(define (files files) files)

;;;; Defaults

(define (base-libraries name)
  `(,(directory name '())))

(define (base-tests)
  `(,(directory "tests" '())))

(define (base-programs name)
  `(,(directory "bin"
                `(,(file name 'scheme #f "")))))

(define (base-documentation name)
  `(,(file "README" 'txt #f "")
    ,(file "HACKING" 'txt #f "")
    ,(file "COPYING" 'txt #f "")
    ,(directory "doc"
                `(,(file name 'texi "texi" "")))))

(define (base-infrastructure)
  `(,(file "guix" 'scheme "scm" "")
    ,(file "halcyon" 'scheme "scm" #f)))

;;;;; Helpers

(define* (context->fname context name #:optional extension)
  (string-join
   (append context (list (if extension
                             (string-append name "." extension)
                             name)))
   file-name-separator-string))

;;;; Directory Constructor

(define (directory name children)
  (lambda (metadata context operation indentation)
    (define (proc child)
      ;; We use this procedure to recurse on our children
      (child metadata (append context (list name)) operation
             (string-append indentation "  ")))
    ;; write is used for generating our specificaiton->scm
    (if (eq? 'write operation)
        `(directory ,name ,(map proc children))
        (let ((fname (context->fname context name)))
          (match operation
            ;; exec is for generating files & folders
            ('exec
             (if (file-exists? fname)
                 (format #t "Skipping: ~a~%" fname)
                 (mkdir fname))
             (for-each proc children))
            ;; show is for doing dry-runs
            ((or 'show _)
             (if (file-exists? fname)
                 (format #t "~aSkipping: ~a~%" indentation fname)
                 (format #t "~aMaking dir: ~a~%" indentation fname))
             (for-each proc children)))))))

;;;;; File Constructor

(define (file name language extension contents)
  (lambda (spec context operation indentation)
    ;; Defined operations:
    ;; - write: emit a scheme representation of the file, but not contents;
    ;;   used for specification->scm.
    ;; - exec: perform operations on file to actually create it; used for file
    ;;   operations.
    ;; - show: print file operation to screen; used for "dry-runs".
    (if (eq? 'write operation)
        `(file ,name ,language ,extension)
        (let ((fname (context->fname context name extension)))
          (match operation
            ('exec
             (if (file-exists? fname)
                 (format #t "Skipping: ~a~%" fname)
                 ;; Halcyon file needs special processing here: its contents
                 ;; are derived from spec here
                 (with-output-to-file fname
                   (lambda _
                     (cond ((eq? name 'halcyon)
                            (pretty-print (specification->scm spec)))
                           ((string? contents)
                            (format #t "~a~%" contents))
                           (else (pretty-print contents)))))))
            ((or 'show _)
             (if (file-exists? fname)
                 (format #t "~aSkipping: ~a~%" indentation fname)
                 (format #t "~aMaking file: ~a~%" indentation fname))))))))

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

;;;; Run Files

(define (instantiate spec context operation)
  (for-each (cute <> spec context operation "  ")
            (specification-files spec)))

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

(define (values->specification nam versio autho copyrigh synopsi descriptio
                               home-pag licens dependencie file)
  (specification
   (name nam) (version versio) (author autho) (copyright copyrigh)
   (synopsis synopsi) (description descriptio) (home-page home-pag)
   (license licens) (dependencies dependencie)
   (files (append
           (base-libraries nam)
           (base-tests)
           (base-programs nam)
           (base-documentation nam)
           (base-infrastructure)
           file))))

(define (specification->scm spec)
  `(halcyon
    ,@(specification->metadata spec)
    (files
     ,@(map (cute <> (specification->metadata spec) '() 'write)
            (specification-files spec)))))

(define (project-root-directory)
  (if (file-exists? "halcyon.scm")
      (getcwd)
      (throw 'hal-project-missing "halcyon.scm file missing in:" (getcwd))))

;;;; Testing

(define %spec
  (values->specification "hal" "0.1" "Alex Sassmannshausen" '(2018)
                         "A guile project manager"
                         "Manage your Guile project fast & easy."
                         "https://github.com/a-sassmannshausen/hal"
                         "gpl3+"
                         `(("guile-config" guile-config))
                         '()))

(instantiate %spec `(,(getcwd)) 'show)

;; (with-output-to-file "/home/alex/tmp/tst.scm"
;;   (lambda _
;;     (pretty-print (specification->scm %spec))))

;; (with-input-from-file "/home/alex/tmp/tst.scm"
;;   (lambda _
;;     (read)))
