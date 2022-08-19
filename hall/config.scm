;;                                                       -*- coding: utf-8 -*-
;;
;; SPDX-FileCopyrightText: 2022 Alex Sassmannshausen <alex@komputilo.eu>
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

(define-module (hall config)
  #:export (guix-feature?
            nls-feature?
            licensing-feature?))

(define guix-feature? (make-parameter #f (lambda (v) (and (boolean? v) v))))

(define nls-feature? (make-parameter #f (lambda (v) (and (boolean? v) v))))

(define licensing-feature? (make-parameter #f (lambda (v) (and (boolean? v) v))))
