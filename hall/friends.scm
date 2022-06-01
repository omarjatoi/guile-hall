;; hall/friends.scm --- friends implementation    -*- coding: utf-8 -*-
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

(define-module (hall friends)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (guix))

(define (guix args)
  (run (string-append "guix " args) "GNU Guix"
       (list
        "Run make targets from the `build' subcommand")))

(define (run cmd name features)
  (match (status:exit-val (system cmd))
    (127
     (format #t "It seems ~a is not installed.
~a is an optional component of Hall, so you don't have to install it, but it
provides the following additional features:
~a~%" name name (string-join (map (cut string-append "- " <>) features) " "))
     (exit 1))
    (0 #t)
    (n (throw 'friends (format #f "~a returned an error code: ~a. Aborting.~%"
                               name n)))))
