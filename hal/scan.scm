;; hal/scan.scm --- scan implementation    -*- coding: utf-8 -*-
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

(define-module (hal scan)
  #:use-module (hal builders)
  #:use-module (hal common)
  #:use-module (hal spec)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (scan-project))

;; We traverse each file category.  For each directory we encounter, we scan
;; and add all files, making best guesses according to extensions.
(define (scan-project spec context operation)
  (let ((new-spec (set-specification-files
                   spec (scm->files (actual->all-files spec context)))))
    (match operation
      ('exec
       (with-output-to-file "halcyon.scm"
         (lambda _
           (pretty-print (specification->scm new-spec)
                         (current-output-port)))))
      ((or 'show _)
       (pretty-print (specification->scm new-spec) (current-output-port))))))

(define (actual->all-files spec context)
  (let ((spec-files (specification-files spec)))
    (define (proc xsr)
      (filter-map (compose derive-filetypes file-system-tree
                           (cut <> spec context 'path ""))
                  (xsr spec-files)))
    `(files
      (libraries ,(proc files-libraries))
      (tests ,(proc files-tests))
      (programs ,(proc files-programs))
      (documentation ,(proc files-documentation))
      (infrastructure ,(proc files-infrastructure)))))

(define derive-filetypes
  ;; Remove the `stat' object for each file in the tree.
  ;; Also attempt to derive filetype for each individual file
  (match-lambda
    (#f #f)                             ; top-level category file removed
    ((name stat)
     (match (stat:type stat)
       ('regular (filetype-derive name)) ; flat file
       ('directory `(directory ,name ()))
       ((or 'symlink 'block-special 'char-special 'fifo 'socket 'unknown)
        (throw 'hal-derive-filetypes "Unsupported file type:"
               (stat:type stat)))))
    ((name stat children ...)            ; directory
     `(directory ,name ,(map derive-filetypes children)))))
