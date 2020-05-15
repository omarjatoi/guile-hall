;; hall/spec.scm --- spec implementation    -*- coding: utf-8 -*-
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

(define-module (hall spec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (<specification>
            specification specification?
            specification-name specification-prefix specification-version
            specification-author specification-copyright
            specification-synopsis specification-description
            specification-home-page specification-license
            specification-dependencies specification-files
            set-specification-files

            <files>
            files files?
            files-libraries files-tests files-programs files-documentation
            files-infrastructure
            set-files-libraries set-files-tests set-files-programs
            set-files-documentation set-files-infrastructure

            specification->metadata specification->files
            specification->files-tree specification->scm

            <filetype>
            filetype filetype?
            filetype-type filetype-extension filetype-language filetype-notes
            filetype->sxml sxml->filetype
            filetypes-register

            filetype-find

            symlink-filetype directory-filetype log-file-filetype
            test-result-filetype scheme-filetype text-filetype info-filetype
            tex-filetype texi-filetype shell-filetype autoconf-filetype
            automake-filetype in-filetype m4-filetype c-filetype
            compiled-scheme-filetype org-filetype xml-filetype))

;;;; Spec Definition

;; A record type that fills the files field of a <specification>: a container
;; for the different file categories we use in a Guile project.
(define-immutable-record-type <files>
  (files libraries tests programs documentation infrastructure)
  files?
  (libraries files-libraries set-files-libraries)
  (tests files-tests set-files-tests)
  (programs files-programs set-files-programs)
  (documentation files-documentation set-files-documentation)
  (infrastructure files-infrastructure set-files-infrastructure))

;; A container for the fundamental specification used by Hall.
(define-immutable-record-type <specification>
  (specification name prefix version author copyright synopsis description
                 home-page license dependencies files)
  specification?
  (name specification-name)
  (prefix specification-prefix)
  (version specification-version)
  (author specification-author)
  (copyright specification-copyright)
  (synopsis specification-synopsis)
  (description specification-description)
  (home-page specification-home-page)
  (license specification-license)
  (dependencies specification-dependencies)
  (files specification-files set-specification-files))

;; Reduce the noise!
(set-record-type-printer!
 <specification>
 (lambda (spec port)
   (format port "#<<specification> name: ~s version: ~s author: ~s>"
           (specification-name spec) (specification-version spec)
           (specification-author spec))))

;;;; Specification->metadata

(define (specification->metadata spec)
  "Return an SXML style association list containing all the metadata of the
hall specification SPEC."
  `((name ,(specification-name spec))
    (prefix ,(specification-prefix spec))
    (version ,(specification-version spec))
    (author ,(specification-author spec))
    (copyright ,(specification-copyright spec))
    (synopsis ,(specification-synopsis spec))
    (description ,(specification-description spec))
    (home-page ,(specification-home-page spec))
    (license ,(specification-license spec))
    (dependencies ,(specification-dependencies spec))))

(define (specification->files spec)
  "Return an SXML style association list containing the files section of the
hall specification SPEC."
  (let ((spec-files (specification-files spec)))
    (define (proc xsr)
      (map (cut <> spec '() 'write "") (xsr spec-files)))
    `(files
      (libraries ,(proc files-libraries))
      (tests ,(proc files-tests))
      (programs ,(proc files-programs))
      (documentation ,(proc files-documentation))
      (infrastructure ,(proc files-infrastructure)))))

(define (specification->files-tree spec)
  "Return a list of all files, in hall format, contained by the files section
of the hall specification SPEC."
  (let lp ((todo (sort (apply append (map second (cdr (specification->files spec))))
                       (lambda (x y) (string<=? (second x) (second y)))))
           (result '()))
    (match todo
      (() (reverse result))
      ((last) (reverse (cons last result)))
      ((('directory name1 children1) ('directory name2 children2) . rest)
       (if (string=? name1 name2)
           (lp rest (cons `(directory ,name1 ,(append children1 children2))
                          result))
           (lp (cons `(directory ,name2 ,children2) rest)
               (cons `(directory ,name1 ,children1) result))))
      ((first . rest)
       (lp rest (cons first result))))))

(define (specification->scm spec)
  "Return a simple sxml type representation of the hall specification SPEC."
  `(hall-description
    ,@(specification->metadata spec)
    ,(specification->files spec)))

;;;; filetypes

(define-record-type <filetype>
  (filetype type extension language notes)
  filetype?
  (type filetype-type)
  (extension filetype-extension)
  (language filetype-language)
  (notes filetype-notes))

(define (sxml->filetype spec)
  "Return a filetype record derived from SPEC. SPEC is a list of 3 ordered
elements: the symbol type, the string or #f extension, and the symbol
language."
  (apply filetype spec))

(define (filetype->sxml ft)
  "Return an sxml representation of the filetype FT."
  (list
   (filetype-type ft)
   (filetype-extension ft)
   (filetype-language ft)))

(define symlink-filetype (filetype 'symlink "" 'symlink '()))

(define directory-filetype (filetype 'directory "" 'directory '()))

(define log-file-filetype (filetype 'log-file "log" 'text '()))

(define test-result-filetype (filetype 'test-result-file "trs" 'text '()))

(define scheme-filetype (filetype 'scheme-file "scm" 'scheme '()))

(define text-filetype (filetype 'text-file #f 'text '()))

(define info-filetype (filetype 'info-file "info" 'info '()))

(define tex-filetype (filetype 'tex-file "tex" 'texinfo '()))

(define texi-filetype (filetype 'texi-file "texi" 'texinfo '()))

(define shell-filetype (filetype 'shell-file "sh" 'shell '()))

(define autoconf-filetype (filetype 'autoconf-file "ac" 'autoconf '()))

(define automake-filetype (filetype 'automake-file "am" 'automake '()))

(define in-filetype (filetype 'in-file "in" #f '()))

(define m4-filetype (filetype 'm4-file "m4" 'm4 '()))

(define c-filetype (filetype 'c-file "c" 'c
                             '("
Your project includes C files. Unfortunately Hall does not yet support
autogenerating build infrastructure files that include C files. You will have
to tweak your configure.ac and automake.am files yourself.
")))

(define compiled-scheme-filetype
  (filetype 'compiled-scheme-file "go" 'go '()))

(define org-filetype (filetype 'org-file "org" 'org '()))

(define xml-filetype (filetype 'xml-file "xml" 'xml '()))

;; ft

(define filetypes-register
  (make-parameter
   (list symlink-filetype directory-filetype log-file-filetype
         test-result-filetype scheme-filetype text-filetype info-filetype
         tex-filetype texi-filetype shell-filetype autoconf-filetype
         automake-filetype in-filetype m4-filetype c-filetype
         compiled-scheme-filetype org-filetype xml-filetype)))

(define* (filetype-find pred #:optional (accessor filetype-type))
  (find (compose (cut and=> <> pred) accessor) (filetypes-register)))
