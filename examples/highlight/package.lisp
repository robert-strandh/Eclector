(cl:defpackage #:eclector.examples.highlight
  (:use
   #:cl)

  (:shadow
   #:package)

  (:local-nicknames
   (#:a #:alexandria))

  ;; Concrete syntax tree protocol
  (:export
   #:source
   #:start
   #:end

   #:object
   #:parent
   #:children

   #:name
   #:package
   #:intern?

   #:message)

  (:export
   #:highlight))
