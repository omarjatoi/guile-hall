(use-modules
 (guix packages)
 (guix licenses)
 (guix download)
 (guix build-system gnu)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages pkg-config)
 (gnu packages texinfo))

(package
  (name "guile-hall")
  (version "0.1.1")
  (source "./guile-hall-0.1.1.tar.gz")
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
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (bin (string-append out "/bin/"))
                  (site (string-append out "/share/guile/site")))
             (match (scandir site)
               (("." ".." version)
                (let ((modules (string-append site "/" version))
                      (compiled-modules
                       (string-append
                        out
                        "/lib/guile/"
                        version
                        "/site-ccache")))
                  (for-each
                   (lambda (file)
                     (wrap-program
                         (string-append bin file)
                       `("GUILE_LOAD_PATH" ":" prefix (,modules))
                       `("GUILE_LOAD_COMPILED_PATH"
                         ":"
                         prefix
                         (,compiled-modules))))
                   ,(list 'list "hall"))
                  #t)))))))))
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-2.2)))
  (propagated-inputs
   `(("guile-config" ,guile-config)))
  (synopsis "Guile project tooling")
  (description
   "Hall is a command-line application and a set of Guile libraries that allow you to quickly create and publish Guile projects.  It allows you to transparently support the GNU build system, manage a project hierarchy & provides tight coupling to Guix.")
  (home-page
   "https://gitlab.com/a-sassmannshausen/guile-hall")
  (license gpl3+))

