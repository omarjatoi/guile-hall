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

(define %version "0.5.0")

(define %author "Alex Sassmannshausen")

(define %license 'gpl3+)

(define %copyright '(2018 2020 2021 2022 2023))

(define %gettext-domain "guile-hall")

(define G_ identity)

(define N_ identity)

(define (init-nls) "Dummy as no NLS is used" #t)

(define (init-locale)
  "Dummy as no NLS is used"
  #t)

