;; guile-hal/hal.scm --- hal implementation    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2018 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;;
;; This file is part of guile-guile-hal.
;;
;; guile-guile-hal is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; guile-guile-hal is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with guile-guile-hal; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;;; Code:

(define-module (hal)
  #:use-module (hal common)
  #:use-module (hal spec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

;;;; Testing

(define %spec
  (values->specification "hal" "0.1" "Alex Sassmannshausen" '(2018)
                         "A guile project manager"
                         "Manage your Guile project fast & easy."
                         "https://github.com/a-sassmannshausen/hal"
                         "gpl3+"
                         `(("guile-config" guile-config))
                         '()))

(instantiate %spec `(,(getcwd)) 'show)
