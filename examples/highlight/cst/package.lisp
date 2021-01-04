(cl:defpackage #:eclector.examples.highlight.cst
  (:use
   #:cl)

  (:local-nicknames
   (#:a #:alexandria))

  (:shadow
   #:package)

  ;; Concrete syntax tree protocol
  (:export
   #:source
   #:start
   #:end

   ;; Object node protocol
   #:object

   ;; Node hierarchy protocol
   #:parent
   #:children

   ;; Error node protocol
   #:message

   ;; Symbol node protocol
   #:name
   #:package
   #:internp)

  ;; Node classes
  (:export
   #:node

   #:root-node
   #:make-cst

   #:syntax-error
   #:make-syntax-error

   #:skipped-node
   #:comment-node
   #:line-comment-node
   #:block-comment-node

   #:quote-node
   #:quasiquote-node
   #:unquote-node
   #:function-node

   #:feature-expression-node

   #:number-node
   #:character-node

   #:symbol-node
   #:uninterned-symbol-node
   #:keyword-symbol-node
   #:interned-symbol-node
   #:standard-symbol-node
   #:lambda-list-keyword-symbol-node

   #:structure-node
   #:array-node
   #:sequence-node
   #:string-node
   #:vector-node
   #:cons-node
   #:pathname-node))
