;; tests/common.scm --- common implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2021 Alex Sassmannshausen <alex@pompo.co>
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
;; Source-file: hall/common.scm
;;
;;; Code:

(define-module (tests common)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (hall common)
  #:use-module (hall spec))

(test-begin "common")

;;;;; Tests for: file->filepath
(test-begin "file->filepath")

(test-equal "scheme-file resolution"
  "tests/test.scm"
  ((@@ (hall common) file->filepath) 'scheme-file "test" '("tests")))

(test-equal "unknown file resolution"
  "tests/test"
  ((@@ (hall common) file->filepath) 'eduiranedu "test" '("tests")))

(test-end "file->filepath")

;;;;; Tests for: filetype-read
(test-begin "filetype-read")

(test-equal "scheme-file read"
  "tests/test.scm"
  ((filetype-read 'scheme-file "test" "tests/test.scm" (make-hash-table)) '()
   '("tests") 'path ""))

(test-equal "unknown-file read"
  "tests/test"
  ((filetype-read 'unknown-type "test" "tests/test.edurinaed"
                  (make-hash-table)) '() '("tests") 'path ""))

(test-end "filetype-read")

(test-end "common")
