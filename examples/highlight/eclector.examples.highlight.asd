(defsystem "eclector.examples.highlight"
  :description "A system for syntax-highlighting Common Lisp code"
  :license "BSD"
  :author "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on ("alexandria"
               "eclector")
  :serial     t
  :components ((:module     "cst"
                :components ((:file       "package")
                             (:file       "protocol")
                             (:file       "nodes")))

               (:file       "package")
               (:file       "protocol")

               (:file       "read")

               (:module     "render"
                :serial     t
                :components ((:file "package")
                             (:file "protocol")

                             (:file "mixins")

                             (:file "html")
                             (:file "ansi-text")))

               (:file       "interface")))
