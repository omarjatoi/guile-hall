(hall-description
 (name "hall")
 (prefix "guile")
 (version "0.5.0")
 (author "Alex Sassmannshausen")
 (email "alex@komputilo.eu")
 (copyright (2018 2020 2021 2022 2023 2024))
 (synopsis "Guile project tooling")
 (description
  "Hall is a command-line application and a set of Guile libraries that allow you to quickly create and publish Guile projects.  It allows you to transparently support the GNU build system, manage a project hierarchy & provides tight coupling to Guix.")
 (home-page
  "https://gitlab.com/a-sassmannshausen/guile-hall")
 (license gpl3+)
 (dependencies (("guile-config" (config))
                ("guile-lib" (logging logger))))
 (skip ())
 (features
  ((guix #t)
   (use-guix-specs-for-dependencies #t)
   (native-language-support #f)
   (licensing #t)))
 (files (libraries
         ((directory
           "hall"
           ((hall-file "hconfig.scm")
            (scheme-file "hconfig")
            (scheme-file "config")
            (scheme-file "publish")
            (scheme-file "friends")
            (scheme-file "workarounds")
            (scheme-file "build")
            (scheme-file "dist")
            (scheme-file "common")
            (scheme-file "scan")
            (scheme-file "builders")
            (scheme-file "clean")
            (scheme-file "init")
            (scheme-file "logging")
            (scheme-file "spec")))))
        (tests ((directory
                 "tests"
                 ((scheme-file "build")
                  (scheme-file "guix-file")
                  (scheme-file "scan")
                  (scheme-file "common")
                  (scheme-file "hall")
                  (scheme-file "spec")))))
        (programs
         ((directory "scripts" ((in-file "hall")))))
        (documentation
         ((org-file "README")
          (symlink "README" "README.org")
          (text-file "HACKING")
          (text-file "COPYING")
          (text-file "NEWS")
          (text-file "AUTHORS")
          (text-file "ChangeLog")
          (directory "doc" ((texi-file "hall")))))
        (infrastructure
         ((unknown-type ".gitlab-ci.yml")
          (scheme-file "guix")
          (scheme-file "hall")))))
