;; hall/scan.scm --- scan implementation    -*- coding: utf-8 -*-
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

(define-module (hall scan)
  #:use-module (hall builders)
  #:use-module (hall common)
  #:use-module (hall spec)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (scan-project))

;; We traverse each file category.  For each directory we encounter, we scan
;; and add all files, making best guesses according to extensions.
(define (scan-project spec context skip operation)
  "Commandline tool for scanning & returning and updated hall.scm file of the
state of a given project.  SPEC is a hall specification file for the project
in question.  CONTEXT is a list containing as its first and only element the
absolute filepath to the project base-directory.  SKIP is a list of relative
(to the project root directory) filepaths to be ignored by scan-project.
OPERATION can be 'show or 'exec."
  (let ((new-spec (set-specification-files
                   spec (scm->files (actual->all-files spec context skip)
                                    (specification-name spec)))))
    (match operation
      ('exec
       (with-output-to-file "hall.scm"
         (lambda _
           (pretty-print (specification->scm new-spec)
                         (current-output-port)))))
      ((or 'show _)
       (format #t "Dryrun:~%")
       (pretty-print (specification->scm new-spec) (current-output-port))
       (format #t "Finished dryrun.~%")))))

(define (actual->all-files spec context skip)
  "Return an SXML style association list containing a representation of the
actual state of the folder hierarchy of the project described by SPEC &
CONTEXT.  SPEC is a hall specification file for the project
in question.  CONTEXT is a list containing as its first and only element the
absolute filepath to the project base-directory.  SKIP is a list of relative
(to the project root directory) filepaths to be ignored by scan-project."
  (let ((spec-files (specification-files spec)))
    (define (proc candidates)
      (filter-map (compose (cut derive-filetypes <> skip context)
                           file-system-tree (cute <> spec context 'path ""))
                  candidates))
    `(files
      (libraries ,(proc (files-libraries spec-files)))
      (tests ,(proc (files-tests spec-files)))
      (programs ,(proc (files-programs spec-files)))
      (documentation ,(proc (append (files-documentation spec-files)
                                    (base-autotools-documentation))))
      (infrastructure ,(proc (append (files-infrastructure spec-files)
                                     (base-autotools-infrastructure)))))))

(define (derive-filetypes file skip context)
  "Return a hall format file descriptor for the file or folder FILE, or #f if
that file or folder is in the list of relative paths SKIP.  CONTEXT is a list
containing as its first and only element the absolute filepath to the project
base-directory."
  (let lp ((file file)
           (path context))
    ;; Remove the `stat' object for each file in the tree.
    ;; Also attempt to derive filetype for each individual file
    (match file
      (#f #f)                             ; top-level category file removed
      ((name stat)
       (match (stat:type stat)
         ('regular                        ; flat file
          (and (not (blacklisted? (string-join (reverse (cons name path))
                                               file-name-separator-string)
                                  (first context) skip))
               (filetype-derive name)))
         ('directory `(directory ,name ()))
         ((or 'symlink 'block-special 'char-special 'fifo 'socket 'unknown)
          (throw 'hall-derive-filetypes "Unsupported file type:"
                 (stat:type stat)))))
      ((name stat children ...)            ; directory
       `(directory ,name ,(filter-map (cut lp <> (cons name path))
                                      children))))))
