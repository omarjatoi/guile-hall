;; hall/builders.scm --- builders implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2018-2020 Alex Sassmannshausen <alex@pompo.co>
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

(define-module (hall builders)
  #:use-module (hall spec)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (file
            slink
            directory

            context->fname full-project-name friendly-project-name

            filetype-write filetype-derive

            emit-notes %global-notes))

(define %global-notes
  (make-hash-table))

(define (register-notes notes)
  (and (not (hash-ref %global-notes notes))
       (hash-set! %global-notes notes 1)))

(define* (emit-notes #:optional (port (current-output-port)))
  (hash-remove! %global-notes '())
  (hash-for-each (λ (k _)
                   (for-each (compose (cut format port "~a~%" <>)
                                      (cut string-trim-both <>))
                             k))
                 %global-notes))

;;;;; Helpers

(define* (context->fname context name #:optional extension)
  "Return an absolute filepath constructed using the list representation
of the files directory at CONTEXT, and the filename of NAME.  If EXTENSION is
a string, add \".EXTENSION\" to the resulting filename."
  (string-join
   (append context (list (if extension
                             (string-append name "." extension)
                             name)))
   file-name-separator-string))

(define* (full-project-name spec)
  "Return the full name of the project described by specification SPEC.  The
full name is the project's prefix and the project's name, separated by a
\"-\"."
  (match (specification-prefix spec)
    ((or #f "") (specification-name spec))
    (prefix (string-append (specification-prefix spec) "-"
                           (specification-name spec)))))

(define* (friendly-project-name spec)
  "Return a human friendly version of the full name of the project described
by the specification SPEC."
  (string-titlecase
   (match (specification-prefix spec)
     ((or #f "") (specification-name spec))
     (prefix (string-append (specification-prefix spec) " "
                            (specification-name spec))))))

;;;; File-like objects
;;
;; The following operations should be defined for all file-like objecst:
;; - write: emit a scheme representation of the file, but not contents;
;;   used for specification->scm.
;; - contents: display the contents of the file.
;; - path: emit the hall path to the file.
;; - exec: perform operations on file to actually create it; used for file
;;   operations.
;; - raw: emit the hall path to the file, or execute the operation on
;;   children, if this is a directory.
;; - raw+contents: like raw, but also emit contents of the file.
;; - show: print file operation to screen; used for "dry-runs".
;; - show-contents: resolve the file to its actual contents.

;;;;; Directory Constructor

(define (directory name children)
  "Return a hall directory procedure with the directory name NAME and a
recursive list of hall directory or file procedures CHILDREN.  Directory
procedures can be called with a number of operations.  See the code below to
find out which."
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
                (format #t "~aSkipping: ~a~%" indentation fname)
                (begin
                  (format #t "~aMaking dir: ~a~%" indentation fname)
                  (mkdir fname)))
            (for-each proc children))
           ;; We use raw to return a list of file paths
           ('raw (map proc children))
           ('raw+contents (map proc children))
           ;; show is for doing dry-runs
           ;; default, so also show-contents.
           ((or 'show _)
            (if (file-exists? fname)
                (format #t "~aSkipping: ~a~%" indentation fname)
                (format #t "~aMaking dir: ~a~%" indentation fname))
            (for-each proc children))))))))

;;;;; File Constructor

(define (file name filetype contents)
  "Return a hall file procedure with the file name NAME.  FILETYPE is a
filetype record and specifies metadata for the file.  CONTENTS can be a
procedure of one argument (the project's specification) or a string.  In both
cases contents describe the contents of the file.

A number of operations on hall file procedures are possible.  Consult the
commentary above to find out what these are."
  (lambda (spec context operation indentation)
    (match operation
      ('write
       (register-notes (filetype-notes filetype))
       (filetype-write name filetype))
      ('contents contents)
      (_
       (let ((fname (context->fname context name
                                    (filetype-extension filetype))))
         (match operation
           ('path fname)
           ('exec
            (register-notes (filetype-notes filetype))
            (if (file-exists? fname)
                (format #t "~aSkipping: ~a~%" indentation fname)
                (begin
                  (format #t "~aMaking file: ~a~%" indentation fname)
                  (with-output-to-file fname
                    (lambda _
                      (cond ((and (string=? name "hall")
                                  (equal? filetype scheme-filetype))
                             ;; Hall file needs special processing here:
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
           ('raw+contents `(,fname . ,contents))
           ('show-contents
            (register-notes (filetype-notes filetype))
            (cond ((string? contents)
                   (display contents))
                  ((procedure? contents)
                   (contents spec))
                  (else (pretty-print contents))))
           ((or 'show _)
            (register-notes (filetype-notes filetype))
            (if (file-exists? fname)
                (format #t "~aSkipping: ~a~%" indentation fname)
                (format #t "~aMaking file: ~a~%" indentation fname)))))))))

;;;;; Symlink Constructor

(define (slink name target)
  "Return a hall symlink procedure with the file name NAME, that will point to
TARGET.

A number of operations on hall file procedures are possible.  Consult the
commentary above to find out what these are."
  (lambda (spec context operation indentation)
    (match operation
      ('write `(symlink ,name ,target))
      ('contents (string-append "This is a symlink to " target))
      (_
       (let ((fname (context->fname context name))
             (ftarget (context->fname context target)))
         (match operation
           ('path fname)
           ('exec
            (if (file-exists? fname)
                (format #t "~aSkipping: ~a~%" indentation fname)
                (begin
                  (format #t "~aLinking file: ~a -> ~a~%" indentation fname
                          ftarget)
                  (symlink target fname))))
           ('raw fname)
           ('raw+contents `(,fname . ,(string-append "This is a symlink to "
                                                     ftarget)))
           ('show-contents
            `(,fname . ,(string-append "This is a symlink to " ftarget)))
           ((or 'show _)
            (if (file-exists? fname)
                (format #t "~aSkipping: ~a~%" indentation fname)
                (format #t "~aLinking file: ~a -> ~a~%" indentation fname
                        ftarget)))))))))

;;;; Filetype converters

(define (filetype-write name filetype)
  "Return an SXML representation of the hall description of the file with name
NAME, in LANGUAGE and with EXTENSION."
  `(,(filetype-type filetype) ,name))

(define (filetype-derive name stat)
  "Return an SXML representation of the file with filename NAME, by analysing
its extension."
  (if (eqv? (stat:type stat) 'symlink)
      `(symlink ,name ,(readlink name))
      (match (string-match "^(.+)\\.(.*)$" name)
        (#f `(text-file ,name))
        (m (match (filetype-find (cute string=? <> (match:substring m 2))
                                 filetype-extension)
             ((? unknown-filetype? ft) (list (filetype-type ft) name))
             (ft (list (filetype-type ft) (match:substring m 1))))))))
