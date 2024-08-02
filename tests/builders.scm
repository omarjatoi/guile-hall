;;                                                       -*- coding: utf-8 -*-
;;
;; SPDX-FileCopyrightText: 2024 Alex Sassmannshausen <alex@komputilo.eu>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is part of guile-hall.
;;
;; guile-hall is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; guile-hall is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
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

(define-module (tests builders)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (hall builders)
  #:use-module (hall spec))

(test-begin "Builders")

(test-begin "full-project-name")

(test-equal "No prefix, no postfix"
  "project"
  (full-project-name (specification "project" "" "" #f #f #f #f #f #f #f #f #f
                                    #f #f #f)))

(test-equal "No prefix"
  "project-postfix"
  (full-project-name (specification "project" "" "postfix" #f #f #f #f #f #f #f
                                    #f #f #f #f #f)))

(test-equal "No postfix"
  "prefix-project"
  (full-project-name (specification "project" "prefix" "" #f #f #f #f #f #f #f
                                    #f #f #f #f #f)))

(test-equal "All the things"
  "prefix-project-postfix"
  (full-project-name (specification "project" "prefix" "postfix" #f #f #f #f #f
                                    #f #f #f #f #f #f #f)))

(test-end "full-project-name")

(test-begin "friendly-project-name")

(test-equal "No prefix, no postfix"
  "Project"
  (friendly-project-name (specification "project" "" "" #f #f #f #f #f #f #f #f #f
                                        #f #f #f)))

(test-equal "No prefix"
  "Project Postfix"
  (friendly-project-name (specification "project" "" "postfix" #f #f #f #f #f #f #f
                                        #f #f #f #f #f)))

(test-equal "No postfix"
  "Prefix Project"
  (friendly-project-name (specification "project" "prefix" "" #f #f #f #f #f #f #f
                                        #f #f #f #f #f)))

(test-equal "All the things"
  "Prefix Project Postfix"
  (friendly-project-name (specification "project" "prefix" "postfix" #f #f #f #f #f
                                        #f #f #f #f #f #f #f)))

(test-end "friendly-project-name")

(test-end "Builders")
