(cl:defpackage #:eclector.examples.highlight
  (:use
   #:cl)

  (:shadow
   #:package)

  (:local-nicknames
   (#:a   #:alexandria)

   (#:cst #:eclector.examples.highlight.cst))

  (:export
   #:highlight))
