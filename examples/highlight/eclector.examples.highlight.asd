(defsystem "eclector.examples.highlight"
  :description "A system for syntax-highlighting Common Lisp code"
  :license "BSD"
  :author "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on ("alexandria"
               "eclector")
  :serial     t
  :components ((:file "package")
               (:file "cst")
               (:file "parse")
               (:file "html")
               (:file "ansi-text")))
