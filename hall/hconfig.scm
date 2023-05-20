(define-module
  (hall hconfig)
  #:use-module
  (srfi srfi-26)
  #:export
  (%version
    %author
    %license
    %copyright
    %gettext-domain
    G_
    N_
    init-nls
    init-locale))

(define %version "0.4.1")

(define %author "Alex Sassmannshausen")

(define %license 'gpl3+)

(define %copyright '(2018 2020 2021 2022 2023))

(define %gettext-domain "guile-hall")

(define G_ (cut gettext <> %gettext-domain))

(define N_
  (cut ngettext <> <> <> %gettext-domain))

(define (init-nls)
  "Bind this project's textdomain."
  (bindtextdomain %gettext-domain "/usr/local/share/locale"))

(define (init-locale)
  "Install the current locale settings."
  (catch 'system-error
         (lambda _ (setlocale LC_ALL ""))
         (lambda args
           (false-if-exception
             (setlocale LC_ALL "en_US.utf8"))))
  (init-nls)
  (textdomain %gettext-domain))

