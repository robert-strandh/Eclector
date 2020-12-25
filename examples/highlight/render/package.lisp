(cl:defpackage #:eclector.examples.highlight.render
  (:use
   #:cl)

  (:shadow
   #:stream)

  (:local-nicknames
   (#:a   #:alexandria)

   (#:cst #:eclector.examples.highlight))

  ;; Render client protocol
  (:export
   #:write-character
   #:enter-node
   #:leave-node
   #:enter-errors
   #:leave-errors)

  ;; Entry point
  (:export
   #:render))
