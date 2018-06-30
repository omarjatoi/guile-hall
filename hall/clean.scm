;; hall/clean.scm --- clean implementation    -*- coding: utf-8 -*-
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

(define-module (hall clean)
  #:use-module (hall builders)
  #:use-module (hall common)
  #:use-module (hall spec)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (clean-project))

(define (clean-project spec context skip operation)
  "Commandline tool for cleaning a project's folder-hierarchy.  SPEC is a hall
specification file for the project in question.  CONTEXT is a list containing
as its first and only element the absolute filepath to the project
base-directory.  SKIP is a list of relative (to the project root directory)
filepaths to be ignored by clean-project.  OPERATION can be 'show or 'exec."
  (when (eq? 'show operation)
    (format #t "Dryrun:~%"))
  (receive (delete others)
      (partition (match-lambda (('delete . rest) #t) (_ #f))
                 (project-walk (specification->files-tree spec)
                               (first context) skip))
    (format #t "  Deleting:~%")
    (for-each
     (compose (lambda (path)
                ;; I'm real paranoid about deleting the wrong files!
                (if (and (string=? (getcwd) (first context))
                         (string=? (basename (getcwd))
                                   (full-project-name spec))
                         (string=? (substring path 0 (string-length (getcwd)))
                                   (getcwd)))
                    (begin
                      (format #t "~0,1,4@a~%" path)
                      (when (eq? 'exec operation)
                        (system* "rm" "-rf" path)))
                    (throw 'clean-project
                           "Filepath/project mismatch. Won't delete nothing."
                           (getcwd)
                           (string-append (dirname (getcwd))
                                          file-name-separator-string
                                          (full-project-name
                                           spec)))))
              second)
     delete)
    (receive (skip keep)
        (partition (compose (cut eq? 'skip <>) first) others)
      (format #t "~%  Skipping:~%")
      (for-each (compose (cute format #t "~0,1,4@a~%" <>) second) skip)
      (format #t "~%  Keeping:~%")
      (for-each (compose (cute format #t "~0,1,4@a~%" <>) second) keep)))
  (when (eq? 'show operation)
    (format #t "Finished dryrun.~%")))

(define (project-walk files project-root skip)
  "Return a list of operations matched to file paths describing what needs to
be done to clean the hal project at PROJECT-ROOT, assuming the hal style files
description FILES is complete, & ignoring the relative filepaths contained in
the list SKIP."
  (define (shrink-path path)
    (string-split (string-drop path (1+ (string-length project-root)))
                  (string->char-set file-name-separator-string)))
  (define (project-root? path)
    (string=? path project-root))
  (reverse
   (file-system-fold
    (lambda (path _ -)                  ; enter?
      (and (not (blacklisted? path project-root skip))
           (or (project-root? path)
               (not (null? (dir-match (shrink-path path) files))))))
    (lambda (path stat result)          ; leaf
      ;; When we hit a leaf we want to check our spec for the existence of
      ;; that leaf & perform an operation against it.
      (cons (cond ((blacklisted? path project-root skip) `(skip ,path))
                  ((file-match (shrink-path path) files) `(keep ,path))
                  (else `(delete ,path)))
            result))
    (lambda (_ - result) result)        ; down
    (lambda (_ - result) result)        ; up
    (lambda (path stat result)          ; skip
      (cons (if (blacklisted? path project-root skip)
                `(skip ,path)
                `(delete ,path))
            result))
    (lambda (path stat result)          ; error
      ;; No error handling at present.
      result)
    '()
    project-root)))

(define (find-dir target candidates)
  "Return #t if the name TARGET can be matched to a directory in CANDIDATES.
Return #f otherwise."
  (filter-map (match-lambda
                (('directory (? (cute string=? target <>)) children) children)
                (_ #f))
              candidates))

(define (dir-match cropped candidates)
  "Return #t if the description of the actually existing directory CROPPED can
be matched to a directory in our hal representation of the files of the
project in CANDIDATES.  Return #f otherwise."
  (let lp ((breadcrumbs cropped)
           (candidates candidates))
    (match breadcrumbs
      ((dir) (find-dir dir candidates))
      ((dir . rest)
       (match (find-dir dir candidates)
         (#f #f)
         (children (lp rest children)))))))

(define (file-match cropped candidates)
  "Return #t if the description of the actually existing file CROPPED can be
matched to a file in our hal representation of the files of the project in
CANDIDATES.  Return #f otherwise."
  (let lp ((breadcrumbs cropped)
           (candidates candidates))
    (match breadcrumbs
      ((needle) 
       (find (cute equal? (filetype-derive needle) <>) candidates))
      ((dir . rest)
       (match (find-dir dir candidates)
         (#f (throw 'hall-file-match "Should not have happened."))
         (() #f)
         (((children ...)) (lp rest children)))))))
