;; hal/builders.scm --- builders implementation    -*- coding: utf-8 -*-
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

(define-module (hal builders)
  #:use-module (hal spec)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (file
            directory

            context->fname full-project-name

            filetype-write filetype-derive))

;;;;; Helpers

(define* (context->fname context name #:optional extension)
  (string-join
   (append context (list (if extension
                             (string-append name "." extension)
                             name)))
   file-name-separator-string))

(define* (full-project-name spec)
  (string-append (specification-prefix spec) (specification-name spec)))

;;;; Directory Constructor

(define (directory name children)
  (lambda (spec context operation indentation)
    (define (proc child)
      ;; We use this procedure to recurse on our children
      (child spec (append context (list name)) operation
             (string-append indentation "  ")))
    ;; write is used for generating our specificaiton->scm
    (match operation
      ('write `(directory ,name ,(map proc children)))
      ('contents #f)
      (_
       (let ((fname (context->fname context name)))
         (match operation
           ('path fname)
           ;; exec is for generating files & folders
           ('exec
            (if (file-exists? fname)
                (format #t "Skipping: ~a~%" fname)
                (begin
                  (format #t "~aMaking dir: ~a~%" indentation fname)
                  (mkdir fname)))
            (for-each proc children))
           ;; We use raw to return a list of file paths
           ('raw (map proc children))
           ;; show is for doing dry-runs
           ((or 'show _)
            (if (file-exists? fname)
                (format #t "~aSkipping: ~a~%" indentation fname)
                (format #t "~aMaking dir: ~a~%" indentation fname))
            (for-each proc children))))))))

;;;;; File Constructor

(define (file name language extension contents)
  (lambda (spec context operation indentation)
    ;; Defined operations:
    ;; - write: emit a scheme representation of the file, but not contents;
    ;;   used for specification->scm.
    ;; - exec: perform operations on file to actually create it; used for file
    ;;   operations.
    ;; - show: print file operation to screen; used for "dry-runs".
    (match operation
      ('write (filetype-write name language extension))
      ('contents contents)
      (_
       (let ((fname (context->fname context name extension)))
         (match operation
           ('path fname)
           ('exec
            (if (file-exists? fname)
                (format #t "Skipping: ~a~%" fname)
                (begin
                  (format #t "~aMaking file: ~a~%" indentation fname)
                  (with-output-to-file fname
                    (lambda _
                      (cond ((string=? name "halcyon")
                             ;; Halcyon file needs special processing here:
                             ;; its contents are derived from spec here
                             (pretty-print (specification->scm spec)
                                           (current-output-port)))
                            ((string? contents)
                             (display contents))
                            ((procedure? contents)
                             (contents spec))
                            (else
                             (pretty-print contents))))))))
           ('raw fname)
           ('show-contents
            (cond ((string? contents)
                   (display contents))
                  ((procedure? contents)
                   (contents spec))
                  (else (pretty-print contents))))
           ((or 'show _)
            (if (file-exists? fname)
                (format #t "~aSkipping: ~a~%" indentation fname)
                (format #t "~aMaking file: ~a~%" indentation fname)))))))))

;;;; Filetype converters

(define (filetype-write name language extension)
  (match (cons language extension)
    (('scheme . "scm") `(scheme-file ,name))
    (('text . #f) `(text-file ,name))
    (('info . "info") `(info-file ,name))
    (('texinfo . "texi") `(texi-file ,name))
    (('shell . "sh") `(shell-file ,name))
    (('autoconf . "ac") `(autoconf-file ,name))
    (('automake . "am") `(automake-file ,name))
    ((_ . "in") `(in-file ,name))
    ((compiled-scheme . "go") `(compiled-scheme-file ,name))
    (_ `(file ,name ,language ,extension))))

(define (filetype-derive name)
  (let ((matches (string-match "(.*)\\.(.*)" name)))
    (if matches
        (match (map (cut match:substring matches  <>)
                    '(1 2))
          ((name "scm") `(scheme-file ,name))
          ((name "texi") `(texi-file ,name))
          ((name "info") `(info-file ,name))
          ((name "sh") `(shell-file ,name))
          ((name "ac") `(autoconf-file ,name))
          ((name "am") `(automake-file ,name))
          ((name "in") `(in-file ,name))
          ((name "go") `(compiled-scheme-file ,name))
          ((name ext) `(unknown-file ,(string-append name "." ext))))
        `(text-file ,name))))
