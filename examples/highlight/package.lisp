(cl:defpackage #:eclector.examples.highlight
  (:use
   #:cl)

  (:shadow
   #:package
   #:stream)

  (:local-nicknames
   (#:a #:alexandria))

  (:export
   #:highlight))
