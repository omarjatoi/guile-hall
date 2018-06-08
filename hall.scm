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
         ((scheme-file "hall") (directory "hall" ())))
        (tests ((directory "tests" ())))
        (programs ((directory "scripts" ())))
        (documentation
         ((text-file "README")
          (text-file "HACKING")
          (text-file "COPYING")
          (directory "doc" ((texi-file "hall")))))
        (infrastructure
         ((scheme-file "guix") (scheme-file "hall")))))
