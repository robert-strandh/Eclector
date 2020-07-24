(defsystem "eclector.examples.highlight"
  :depends-on ("eclector")
  :serial     t
  :components ((:file "package")
               (:file "cst")
               (:file "parse")
               (:file "html")))
