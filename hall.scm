(hall-description
 (name "hall")
 (prefix "guile")
 (version "0.1")
 (author "Alex Sassmannshausen")
 (copyright (2018))
 (synopsis "")
 (description "")
 (home-page "")
 (license gpl3+)
 (dependencies
  `(("guix" ,guix) ("guile-config" ,guile-config)))
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
            (scheme-file "guix")))))
        (tests ((directory "tests" ())))
        (programs
         ((directory "scripts" ((in-file "hall")))))
        (documentation
         ((text-file "README")
          (text-file "HACKING")
          (text-file "COPYING")
          (directory "doc" ((texi-file "hall")))))
        (infrastructure
         ((scheme-file "guix") (scheme-file "hall")))))
