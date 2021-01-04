(cl:defpackage #:eclector.examples.highlight.render
  (:use
   #:cl)

  (:shadow
   #:stream)

  (:local-nicknames
   (#:a   #:alexandria)

   (#:cst #:eclector.examples.highlight.cst))

  ;; Render client protocol
  (:export
   #:write-character
   #:enter-node
   #:leave-node
   #:enter-errors
   #:leave-errors)

  ;; Stream-based client protocol
  (:export
   #:stream)

  ;; Entry point
  (:export
   #:render))
