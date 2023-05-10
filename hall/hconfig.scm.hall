;;                                                       -*- coding: utf-8 -*-
;;
;; SPDX-FileCopyrightText: 2023 Alex Sassmannshausen <alex@komputilo.eu>
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

(define-module (hall hconfig)
  #:use-module (config licenses)
  #:use-module (srfi srfi-26)
  #:export (%version
            %author
            %license
            %copyright
            %gettext-domain
            G_ N_
            init-nls
            init-locale))

(define %version '@HVERSION@)

(define %author '@AUTHOR@)

(define %license '@LICENSE@)

(define %copyright '@COPYRIGHT@)

(define %gettext-domain
  ;; Text domain for strings used in the tools.
  "guile-hall")

(define G_ (cut gettext <> %gettext-domain))
(define N_ (cut ngettext <> <> <> %gettext-domain))

(define (init-nls)
  (bindtextdomain %gettext-domain "/tmp/hall/share/locale"))

(define (init-locale)
  "Install the current locale settings."
  (catch 'system-error
    (lambda _
      (setlocale LC_ALL ""))
    (lambda args
      ;; We're now running in the "C" locale.  Try to install a UTF-8 locale
      ;; instead.  This one is guaranteed to be available in 'guix' from 'guix
      ;; pull'.
      (false-if-exception (setlocale LC_ALL "en_US.utf8"))))
  (init-nls)
  (textdomain %gettext-domain))
