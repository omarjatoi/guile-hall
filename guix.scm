(use-modules
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (guix build-system gnu)
  (guix download)
  (guix gexp)
  ((guix licenses) #:prefix license:)
  (guix packages)
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
    (list #:modules
          `(((guix build guile-build-system)
             #:select
             (target-guile-effective-version))
            ,@%gnu-build-system-modules)
          #:phases
          (with-imported-modules
            `((guix build guile-build-system)
              ,@%gnu-build-system-modules)
            (gexp (modify-phases
                    %standard-phases
                    (add-after
                      'install
                      'hall-wrap-binaries
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let* ((version (target-guile-effective-version))
                               (site-ccache
                                 (string-append
                                   "/lib/guile/"
                                   version
                                   "/site-ccache"))
                               (site (string-append
                                       "/share/guile/site/"
                                       version))
                               (dep-path
                                 (lambda (env path)
                                   (list env
                                         ":"
                                         'prefix
                                         (cons (string-append
                                                 (ungexp output)
                                                 path)
                                               (map (lambda (input)
                                                      (string-append
                                                        (assoc-ref
                                                          inputs
                                                          input)
                                                        path))
                                                    (list "guile-config"
                                                          "guile-lib"))))))
                               (bin (string-append (ungexp output) "/bin/")))
                          (for-each
                            (lambda (file)
                              (wrap-program
                                (string-append bin file)
                                (dep-path "GUILE_LOAD_PATH" site)
                                (dep-path
                                  "GUILE_LOAD_COMPILED_PATH"
                                  site-ccache)))
                            (list "hall"))))))))))
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
