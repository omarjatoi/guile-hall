(use-modules
  (guix packages)
  ((guix licenses) #:prefix license:)
  (guix download)
  (guix gexp)
  (guix build-system gnu)
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (srfi srfi-1))

(package
  (name "guile-hall")
  (version "0.5.0")
  (source
    (local-file
      (dirname (current-filename))
      #:recursive?
      #t
      #:select?
      (lambda (file stat)
        (not (any (lambda (my-string)
                    (string-contains file my-string))
                  (list ".git" ".dir-locals.el" "guix.scm"))))))
  (build-system gnu-build-system)
  (arguments
    `(#:modules
      ((ice-9 match)
       (ice-9 ftw)
       ,@%gnu-build-system-modules)
      #:phases
      (modify-phases
        %standard-phases
        (add-after
          'install
          'hall-wrap-binaries
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((compiled-dir
                     (lambda (out version)
                       (string-append
                         out
                         "/lib/guile/"
                         version
                         "/site-ccache")))
                   (uncompiled-dir
                     (lambda (out version)
                       (string-append
                         out
                         "/share/guile/site"
                         (if (string-null? version) "" "/")
                         version)))
                   (dep-path
                     (lambda (env modules path)
                       (list env
                             ":"
                             'prefix
                             (cons modules
                                   (map (lambda (input)
                                          (string-append
                                            (assoc-ref inputs input)
                                            path))
                                        ,''("guile-config" "guile-lib"))))))
                   (out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin/"))
                   (site (uncompiled-dir out "")))
              (match (scandir site)
                     (("." ".." version)
                      (for-each
                        (lambda (file)
                          (wrap-program
                            (string-append bin file)
                            (dep-path
                              "GUILE_LOAD_PATH"
                              (uncompiled-dir out version)
                              (uncompiled-dir "" version))
                            (dep-path
                              "GUILE_LOAD_COMPILED_PATH"
                              (compiled-dir out version)
                              (compiled-dir "" version))))
                        ,''("hall"))
                      #t))))))))
  (native-inputs
    (list autoconf automake pkg-config texinfo))
  (inputs (list guile-3.0))
  (propagated-inputs (list guile-config guile-lib))
  (synopsis "Guile project tooling")
  (description
    "Hall is a command-line application and a set of Guile libraries that allow you to quickly create and publish Guile projects.  It allows you to transparently support the GNU build system, manage a project hierarchy & provides tight coupling to Guix.")
  (home-page
    "https://gitlab.com/a-sassmannshausen/guile-hall")
  (license license:gpl3+))

