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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 pretty-print)
  #:export (file directory context->fname scm->specification))

;;;;; Helpers

(define* (context->fname context name #:optional extension)
  (string-join
   (append context (list (if extension
                             (string-append name "." extension)
                             name)))
   file-name-separator-string))

;;;; Directory Constructor

(define (directory name children)
  (lambda (spec context operation indentation)
    (define (proc child)
      ;; We use this procedure to recurse on our children
      (child spec (append context (list name)) operation
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
        (filetype-write name language extension)
        (let ((fname (context->fname context name extension)))
          (match operation
            ('exec
             (if (file-exists? fname)
                 (format #t "Skipping: ~a~%" fname)
                 ;; Halcyon file needs special processing here: its contents
                 ;; are derived from spec here
                 (with-output-to-file fname
                   (lambda _
                     (cond ((string=? name "halcyon")
                            ;; (throw 't (specification->scm spec))
                            (pretty-print (specification->scm spec)
                                          (current-output-port)))
                           ((string? contents)
                            (format #t "~a~%" contents))
                           (else (pretty-print contents)))))))
            ((or 'show _)
             (if (file-exists? fname)
                 (format #t "~aSkipping: ~a~%" indentation fname)
                 (format #t "~aMaking file: ~a~%" indentation fname))))))))

;;;; Halcyon file parser

(define (href scm key)
  (match (assoc-ref scm key)
    ((value) value)
    ((values ...) values)
    (#f (throw 'hal-scm->specification "Missing expected halcyon key:" key))))

(define (category-traverser files)
  (let lp ((files files)
           (accum '()))
    (match files
      (() (reverse accum))
      ;; recurse
      ((('directory name children) . rest)
       (lp rest
           (cons (directory name (lp children '())) accum)))
      (((type name . args) . rest)
       (lp rest
           (cons (apply filetype-read type name args) accum)))
      (_ (throw 'hal-category-traverser "Got muddled:" files accum)))))

(define (scm->specification scm)
  (match scm
    (('halcyon . scm)
     (apply specification
            (append (map (cute href scm <>)
                         '(name version author copyright synopsis description
                                home-page license dependencies))
                    (list
                     (let ((all-files (href scm 'files)))
                       (apply files
                              (map (compose category-traverser
                                            (cute href all-files <>))
                                   '(libraries tests programs documentation
                                               infrastructure))))))))
    (_ (throw 'hal-scm->specification "Invalid halcyon data:" scm))))

;;;; Filetype converters

(define (filetype-write name language extension)
  (match (cons language extension)
    (('scheme . "scm") `(scheme-file ,name))
    (('text . #f) `(text-file ,name))
    (('texinfo . "texi") `(texi-file ,name))
    (('shell . "sh") `(shell-file ,name))
    (_ `(file ,name ,language ,extension))))

(define (filetype-read type name . args)
  (apply file name
         (match type
           ('scheme-file '(scheme "scm" ""))
           ('text-file '(text #f ""))
           ('texi-file '(texinfo "texi" ""))
           ('shell-file '(shell "sh" ""))
           (_ (throw 'hal-filetype-read
                     "Unknown filetype" type)))))

(define (filetype-derive name)
  (match (string-split name #\.)
    ((name "scm") `(scheme-file ,name))
    ((name) `(text-file ,name))
    ((name "texi") `(texi-file ,name))
    ((name "sh") `(shell-file ,name))
    ((name ext) `(file ,name unknown ,ext))))
