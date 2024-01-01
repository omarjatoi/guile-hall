;;; SPDX-FileCopyrightText: 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (hall logging)
  #:use-module (logging logger)
  #:use-module (logging port-log)

  #:use-module (oop goops)
  #:use-module (rnrs enums)

  #:export (setup-logging
            shutdown-logging

            log-level
            make-log-level
            log-debug
            log-info
            log-warning
            log-error))

(define-syntax define-log-level
  ;; This macro defines a log-level enum bound to ENUM-NAME for the
  ;; provided levels, which its constructor bound to ENUM-CONSTRUCTOR.
  ;; It also defines a 'log-LEVEL' procedures to more conveniently log
  ;; at LEVEL.
  (lambda (x)
    (define-syntax-rule (id parts ...)
      ;; Assemble PARTS into a raw (unhygienic) identifier.
      (datum->syntax x (symbol-append (syntax->datum parts) ...)))

    (syntax-case x ()
      ((_ enum-name (level ...) enum-constructor)
       #`(begin
           (define-enumeration enum-name (level ...) enum-constructor)

           #,@(map (lambda (lvl)
                     #`(define (#,(id 'log- lvl) . args)
                         (format #f "Log a message to the `~a' level." '#,lvl)
                         (apply log-msg '#,lvl args)))
                   #'(level ...)))))))

(define-log-level log-level
  (debug info warning error)
  make-log-level)

(define* (setup-logging #:key (level (make-log-level warning)))
  "Configure and open logger at LEVEL, a LOG-LEVEL enum."

  (let* ((lgr        (make <logger>))
         (console    (make <port-log> #:port (current-error-port)))
         (all-levels (enum-set->list (enum-set-universe level)))
         (level-sym  (car (enum-set->list level))))

    ;; Register logging handlers.
    (add-handler! lgr console)

    ;; Configure the log level.
    (let loop ((levels all-levels))
      (let ((level (car levels)))
        (unless (eq? level-sym level)
          (log-debug "disabling logging for level" level)
          (disable-log-level! lgr level)
          (loop (cdr levels)))))

    ;; Activate the configured logger.
    (set-default-logger! lgr)
    (open-log! lgr)
    (log-debug "logging initialized")))

(define (shutdown-logging)
  "Flush and destroy logger."
  (flush-log)
  (close-log!)
  (set-default-logger! #f))
