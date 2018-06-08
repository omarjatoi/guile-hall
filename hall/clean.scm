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
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (clean-project))

(define (clean-project spec context operation)
  (when (eq? 'show operation)
    (format #t "Dryrun:~%"))
  (for-each (match-lambda
              (('keep path)
               (format #t "Keeping: ~a~%" path))
              (('skipped path)
               (format #t "Skipping: ~a~%" path))
              (('delete path)
               ;; I'm real paranoid about deleting the wrong files!
               (if (and (string=? (getcwd) (first context))
                        (string=? (basename (getcwd))
                                  (full-project-name spec))
                        (string=? (substring path 0 (string-length (getcwd)))
                                  (getcwd)))
                   (begin
                     (format #t "Deleting: ~a~%" path)
                     (when (eq? 'exec operation)
                       (system* "rm" "-rf" path)))
                   (throw 'clean-project
                          "Filepath/project mismatch. Won't delete nothing."
                          (getcwd)
                          (string-append (dirname (getcwd))
                                         file-name-separator-string
                                         (full-project-name spec))))))
            (project-walk (specification->files-tree spec)
                          (first context)))
  (when (eq? 'show operation)
    (format #t "Finished dryrun.~%")))

(define (project-walk files project-root)
  (define (shrink-path path)
    (string-split (string-drop path (1+ (string-length project-root)))
                  (string->char-set file-name-separator-string)))
  (define (blacklisted? path)
    ;; Currently only allow blacklisting at the top-level.
    (and (not (project-root? path))
         (member (string-drop path (1+ (string-length project-root)))
                 '(".git"))))
  (define (project-root? path)
    (string=? path project-root))
  (reverse
   (file-system-fold
    (lambda (path _ -)                  ; enter?
      (and (not (blacklisted? path))
           (or (project-root? path) (dir-match (shrink-path path) files))))
    (lambda (path stat result)          ; leaf
      ;; When we hit a leaf we want to check our spec for the existence of
      ;; that leaf & perform an operation against it.
      (cons (cond ((blacklisted? path) `(skipped ,path))
                  ((file-match (shrink-path path) files) `(keep ,path))
                  (else `(delete ,path)))
            result))
    (lambda (_ - result) result)        ; down
    (lambda (_ - result) result)        ; up
    (lambda (path stat result)          ; skip
      (cons (if (blacklisted? path) `(skipped ,path) `(delete ,path)) result))
    (lambda (path stat result)          ; error
      ;; No error handling at present.
      result)
    '()
    project-root)))

(define (find-dir target candidates)
  (find (match-lambda
          (('directory (? (cute string=? target <>)) children) children)
          (_ #f))
        candidates))

(define (dir-match cropped candidates)
  (let lp ((breadcrumbs cropped)
           (candidates candidates))
    (match breadcrumbs
      ((dir) (find-dir dir candidates))
      ((dir . rest)
       (match (find-dir dir candidates)
         (#f #f)
         (children (lp rest children)))))))

(define (file-match cropped candidates)
  (let lp ((breadcrumbs cropped)
           (candidates candidates))
    (match breadcrumbs
      ((needle) 
       (find (cute equal? (filetype-derive needle) <>) candidates))
      ((dir . rest)
       (match (find-dir dir candidates)
         (#f (throw 'hall-file-match "Should not have happened."))
         (dir (lp rest (third dir))))))))
