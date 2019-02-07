(hall-description
 (name "hall")
 (prefix "guile")
 (version "0.2")
 (author "Alex Sassmannshausen")
 (copyright (2018))
 (synopsis "Guile project tooling")
 (description "Hall is a command-line application and a set of Guile libraries that allow you to quickly create and publish Guile projects.  It allows you to transparently support the GNU build system, manage a project hierarchy & provides tight coupling to Guix.")
 (home-page "https://gitlab.com/a-sassmannshausen/guile-hall")
 (license gpl3+)
 (dependencies
  `(("guile-config" (config) ,guile-config "out")))
 (files (libraries
         ((directory
           "hall"
           ((scheme-file "builders")
            (scheme-file "scan")
            (scheme-file "spec")
            (scheme-file "common")
            (scheme-file "dist")
            (scheme-file "clean")
            (scheme-file "init")
            (scheme-file "guix")
            (scheme-file "workarounds")))))
        (tests ((directory "tests" ())))
        (programs
         ((directory "scripts" ((in-file "hall")))))
        (documentation
         ((text-file "README")
          (text-file "HACKING")
          (text-file "COPYING")
          (text-file "NEWS")
          (text-file "AUTHORS")
          (text-file "ChangeLog")
          (directory "doc" ((texi-file "hall")))))
        (infrastructure
         ((scheme-file "guix") (scheme-file "hall")))))
