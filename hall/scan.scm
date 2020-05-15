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
  #:export (scan-project
            add-to-project))

(define (dir->filenames directory-name)
  "Return a list of all the file-paths in DIRECTORY-NAME."
  (match directory-name
    ((name stat)                        ; flat file
     `(,name))
    ((name stat children ...)           ; directory
     ;; Add directory name to results, but then also recurse on children.
     (cons name
           (apply append
                  (map (lambda (child)
                         (map (compose
                               (cut string-join <> file-name-separator-string)
                               (match-lambda
                                 ((? list? n) (cons name n))
                                 (n (list name n))))
                              (dir->filenames child)))
                       children))))))

(define (add-to-project spec context filename section operation)
  "Commandline tool for adding a single file within the project to the
hall.scm file.  SPEC is a hall specification file for the project in question.
CONTEXT is a list containing as its first and only element the absolute
filepath to the project base-directory.  FILENAME is the file to be added.
SECTION is the spec file section to add it to.  OPERATION can be 'show or
'exec."
  (define (tweak files)
    (let lp ((tokens (filter (negate string-null?)
                             (string-split filename #\/)))
             (files (map (cut <> spec context 'write "") files)))
      (let* ((fn (first tokens)))
        (if (= (length tokens) 1)
            ;; We're at the correct depth.  Check if filename already exists
            ;; at this depth.
            (let ((target (filetype-derive fn (stat filename))))
              (if (find (λ (f) (equal? f target)) files)
                  files
                  (cons target files)))
            ;; Incorrect depth.  Does the next level of filename's directory
            ;; exist in spec?
            (match (find (match-lambda
                           (('directory (? (cut string=? <> fn)) _) #t)
                           (_ #f))
                         files)
              (#f
               ;; No, so insert filename here.
               (cons
                (let ((rtokens (reverse tokens)))
                  (fold (λ (next result)
                          `(directory ,next (,result)))
                        (filetype-derive (first rtokens) (stat filename))
                        (cdr rtokens)))
                files))
              ;; Yes, so descend to next level.
              ((_ name children)
               `((directory ,name
                            ,(lp (cdr tokens) children)))))))))
  (when (eqv? (stat:type (stat filename)) 'directory)
    (quit-with-error
     "You cannot currently add directories.  Instead you have to add each
individual file in the directory you wish to add."))
  (let* ((setr (match section
                 ('documentation set-files-documentation)
                 ('programs set-files-programs)
                 ('tests set-files-tests)
                 ('infrastructure set-files-infrastructure)
                 ('libraries set-files-libraries)))
         (xsr (record-accessor <files> section))
         (files (specification-files spec))
         (new-spec (set-specification-files spec (setr (specification-files
                                                        spec)
                                                       (category-traverser
                                                        (tweak (xsr
                                                                files))
                                                        "test")))))
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

;; We traverse each file category.  For each directory we encounter, we scan
;; and add all files, making best guesses according to extensions.
(define (scan-project spec context skip operation)
  "Commandline tool for scanning & returning and updated hall.scm file of the
state of a given project.  SPEC is a hall specification file for the project
in question.  CONTEXT is a list containing as its first and only element the
absolute filepath to the project base-directory.  SKIP is a list of relative
(to the project root directory) filepaths to be ignored by scan-project.
OPERATION can be 'show or 'exec."
  (define (expand entry result)
    "Prepend ENTRY to RESULT, or, if it is a directory, expand ENTRY to all
filenames within it and then prepend it to result."
    (if (and (file-exists? entry)
             (eqv? 'directory (stat:type (stat entry))))
        (append (dir->filenames
                 (file-system-tree (string-trim-right entry #\/)))
                result)
        (cons entry result)))
  (let ((new-spec (set-specification-files
                   spec
                   (scm->files
                    (actual->all-files spec context (fold expand '() skip))
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
    (define (continue? name)
      (not (blacklisted? (string-join (reverse (cons name path))
                                      file-name-separator-string)
                         (first context) skip)))
    (match file
      (#f #f)                             ; top-level category file removed
      ((name stat)
       (and (continue? name)
            (match (stat:type stat)
              ('regular (filetype-derive name stat))
              ('directory `(directory ,name ()))
              ('symlink `(symlink ,name ,(readlink name)))
              ((or 'block-special 'char-special 'fifo 'socket 'unknown)
               (quit-with-error
                "Your project contains a file of a type that is not supported
by Hall yet (~a).  Please report this at our website
(https://gitlab.com/a-sassmannshausen/guile-hall/)."
                (stat:type stat))))))
      ((name stat children ...)            ; directory
       (and (continue? name)
            `(directory ,name ,(filter-map (cut lp <> (cons name path))
                                           children)))))))
