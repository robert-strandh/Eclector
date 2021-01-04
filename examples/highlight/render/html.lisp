(cl:in-package #:eclector.examples.highlight.render)

;;; Utilities

;;; Write CHARACTER to STREAM, replacing CHARACTER with the
;;; appropriate entity, if necessary.
(defun ch (stream character)
  (case character
    (#\& (write-string "&amp;" stream))
    (#\< (write-string "&lt;" stream))
    (#\> (write-string "&gt;" stream))
    (#\" (write-string "&quot;" stream))
    (#\→ (write-string "&rArr;" stream))
    (t   (write-char character stream))))

(defun str (stream string)
  (map nil (a:curry #'ch stream) string))

(defun <span (stream &rest classes)
  (format stream "<span class=\"~{syntax-~A~^ ~}\">" classes))

(defun /span (stream)
  (format stream "</span>"))

(defun <a (stream href &rest classes)
  (format stream "<a href=\"~A\" class=\"\~{syntax-~A~^ ~}\">"
          href classes))

(defun /a (stream)
  (format stream "</a>"))

(defun <nesting-with-depth (stream which depth)
  (format stream "<span class=\"syntax-~A~D\">" which depth))

;;; `link-mixin'
;;;
;;; For clients that add links.

(defclass link-mixin ()
  ())

(defmethod enter-node ((client link-mixin) (node t))
  (a:if-let ((url (url client node)))
    (apply #'<a (stream client) url (a:ensure-list (style-class client node)))
    (call-next-method)))

(defmethod leave-node ((client link-mixin) (node t))
  (a:if-let ((url (url client node)))
    (/a (stream client))
    (call-next-method)))

;;; `bad-whitespace-mixin'
;;;
;;; For clients that highlight bad whitespace.

(defclass bad-whitespace-mixin () ; TODO this should happen by going through the input and creating `syntax-error' instances
  ((%input :initarg :input
           :reader  input)))

(defun trailing-whitespace? (character position input)
  (flet ((whitespace? (character)
           (char= character #\Space)))
    (and (whitespace? character)
         (let ((end-of-line (or (position #\Newline input :start position)
                                (length input))))
           (not (find-if-not #'whitespace? input
                             :start (1+ position) :end end-of-line))))))

(defmethod write-character :around ((client    bad-whitespace-mixin)
                                    (position  t)
                                    (character t)
                                    (node      t))
  (let ((stream (stream client)))
    (cond ((char= character #\Tab)
           (<span stream "bad-whitespace")

           (call-next-method)
           (<span stream "message")
           (write-string "Avoid tab." stream)
           (/span stream)

           (/span stream))
          ((trailing-whitespace? character position (input client))
           (<span stream "bad-whitespace")

           (call-next-method)
           (<span stream "message")
           (write-string "Avoid trailing whitespace." stream)

           (/span stream))
          (t
           (call-next-method)))))

;;; Client

(defclass html-client (stream-mixin
                       nesting-tracking-mixin)
  ())

;;; Defaults

(defmethod style-class ((client html-client) (node t)) ; TODO avoid this
  (let ((name (symbol-name (class-name (class-of node)))))
    (string-downcase (subseq name 0 (- (length name) (length "-node"))))))

(defmethod url ((client t) (node t))
  nil)

(defmethod enter-node ((client html-client) (node t))
  (apply #'<span (stream client) (a:ensure-list (style-class client node))))

(defmethod leave-node ((client html-client) (node t))
  (/span (stream client)))

(defmethod write-character ((client    html-client)
                            (position  t)
                            (character t)
                            (node      t))
  (ch (stream client) character))

;;; Document

(defclass html-document-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into client classes that output
    complete HTML documents."))

(defmethod enter-node ((client html-document-mixin) (node cst:root-node))
  (let ((stream (stream client)))
    (format stream "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" ~
                    \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">~@
                    <html xmlns=\"http://www.w3.org/1999/xhtml\">~@
                      <head>~@
                        <meta charset=\"utf-8\"/>~@
                        <link rel=\"stylesheet\" href=\"test.css\"/>~@
                      </head>~@
                      <body>~@
                        <pre>~%")))

(defmethod leave-node ((client html-document-mixin) (node cst:root-node))
  (let ((stream (stream client)))
    (format stream "   </pre>~@
                      </body>~@
                    </html>~%")))

(defclass html-fragment-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into client classes that output
    HTML fragments for embedding into HTML documents."))

(defmethod enter-node ((client html-fragment-mixin) (node cst:root-node)))

(defmethod leave-node ((client html-fragment-mixin) (node cst:root-node)))

;;;

(defclass minimal-html-client (link-mixin
                               html-fragment-mixin
                               html-client)
  ())

(defclass linking-html-client (link-mixin
                               bad-whitespace-mixin
                               html-document-mixin
                               html-client)
  ())

;;; Skipped

(defmethod style-class ((client html-client) (node cst:skipped-node))
  '("comment"))

(defmethod style-class ((client html-client) (node cst:line-comment-node))
  (list* "line-comment" (call-next-method)))

(defmethod style-class ((client html-client) (node cst:block-comment-node))
  (list* "block-comment" (call-next-method)))

(defmethod enter-node ((client html-client) (node cst:block-comment-node))
  (<nesting-with-depth (stream client)
                       "block-comment"
                       (block-comment-depth client))
  (call-next-method))

(defmethod leave-node ((client html-client) (node cst:block-comment-node))
  (call-next-method)
  (/span (stream client)))

(defmethod write-character ((client    html-client)
                            (position  t)
                            (character t)
                            (node      cst:block-comment-node))
  (let* ((start        (cst:start node))
         (end          (cst:end node))
         (open-start?  (eql position start))
         (open-end?    (eql position (+ start 1)))
         (close-start? (eql position (- end 2)))
         (close-end?   (eql position (1- end)))
         (stream       (stream client)))
    (when open-start?
      (<span stream "open"))
    (when close-start?
      (<span stream "close"))
    (call-next-method)
    (when open-end?
      (/span stream))
    (when close-end?
      (/span stream))))

;;; Quasiquote

(defmethod enter-node ((client html-client) (node cst:quasiquote-node))
  (<nesting-with-depth (stream client)
                       "quasiquote"
                       (quasiquote-depth client))
  (call-next-method))

(defmethod leave-node ((client html-client) (node cst:quasiquote-node))
  (call-next-method)
  (/span (stream client)))

(defmethod enter-node ((client html-client) (node cst:unquote-node))
  (<nesting-with-depth (stream client)
                       "quasiquote"
                       (quasiquote-depth client))
  (call-next-method))

(defmethod leave-node ((client html-client) (node cst:unquote-node))
  (call-next-method)
  (/span (stream client)))

;;; Number

(defmethod style-class ((client html-client) (node cst:number-node))
  "number")

;;; Symbol

(defmethod leave-node :before ((client html-client)
                               (node   cst:interned-symbol-node))
  (when (cst:internp node)
    (let ((stream (stream client)))
      (<span stream "message")
      (write-string "Do not use unexported symbols." stream)
      (/span stream))))

(defmethod style-class ((client html-client) (node cst:interned-symbol-node)) ; TODO return list and use APPEND method combination?
  (if (cst:internp node)
      (list* "two-package-markers" (a:ensure-list (call-next-method)))
      (call-next-method)))

(defmethod url ((client link-mixin) (node cst:standard-symbol-node))
  (format nil "http://l1sp.org/cl/~(~A~)" (cst:name node)))

;;; Sequence

(defmethod enter-node ((client html-client) (node cst:sequence-node))
  (<nesting-with-depth (stream client)
                       "nesting"
                       (mod (nesting-depth client) 10)) ; TODO where to do the mod TODO do mod for kinds of nesting
  (call-next-method))

(defmethod leave-node ((client html-client) (node cst:sequence-node))
  (call-next-method)
  (/span (stream client)))

(defmethod write-character ((client    html-client)
                            (position  t)
                            (character t)
                            (node      cst:sequence-node))
  (let* ((children     (cst:children node))
         (start        (cst:start node))
         (end          (cst:end node))
         (child-start  (a:when-let ((child (first children)))
                         (cst:start child)))
         (child-end    (a:when-let ((child (a:lastcar children)))
                         (cst:end child)))

         (open-start?  (and (eql position start) (not (eql start child-start))))
         (open-end?    (or (and (not child-start) (eql position start))
                           (eql (1+ position) child-start)))

         (close-start? (or (and (not child-end) (eql position (1- end)))
                           (eql position child-end)))
         (close-end?   (and (eql position (1- end)) (not (eql end child-end))))
         (stream       (stream client)))
    (when open-start?
      (<span stream "open"))
    (when close-start?
      (<span stream "close"))
    (call-next-method)
    (when open-end?
      (/span stream))
    (when close-end?
      (/span stream))))

;;; String

(defmethod style-class ((client html-client) (node cst:string-node))
  "string")

;;; Vector

(defmethod style-class ((client html-client) (node cst:vector-node))
  "vector")

;;; Cons

(defmethod style-class ((client html-client) (node cst:cons-node))
  "cons")

;;; Errors

(defmethod enter-errors ((client html-client) (errors sequence))
  (<span (stream client) "error"))

(defmethod leave-errors ((client html-client) (errors sequence))
  (let ((stream (stream client)))
    (<span stream "message")
    (loop :for (error rest) :on errors
          :do (str stream (cst:message error))
          :when rest
          :do (write-string "<br/>" stream))
    (/span stream)

    (/span stream)))
