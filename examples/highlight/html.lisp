(cl:in-package #:eclector.examples.highlight)

;;; Utilities

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

(defclass html-client ()
  ((%stream :initarg :stream
            :reader  stream))
  (:default-initargs
   :stream (a:required-argument :stream)))

;;; Defaults

(defmethod style-class ((client html-client) (node t))
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

(defclass html-document-mixin () ())

(defmethod enter-node ((client html-document-mixin) (node cst))
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

(defmethod leave-node ((client html-document-mixin) (node cst))
  (let ((stream (stream client)))
    (format stream "   </pre>~@
                      </body>~@
                    </html>~%")))

(defclass html-fragment-mixin () ())

(defmethod enter-node ((client html-fragment-mixin) (node cst)))

(defmethod leave-node ((client html-fragment-mixin) (node cst)))

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

(defmethod style-class ((client html-client) (node skipped-node))
  '("comment"))

(defmethod style-class ((client html-client) (node block-comment-node))
  (list* "block-comment" (call-next-method)))

(defmethod style-class ((client html-client) (node line-comment-node))
  (list* "line-comment" (call-next-method)))

;;; Quote

;;; Quasiquote

(defvar *quasiquote-depth* 0)

(defmethod enter-node ((client html-client) (node quasiquote-node))
  (incf *quasiquote-depth*)
  (<nesting-with-depth (stream client) "quasiquote" *quasiquote-depth*)
  (call-next-method))

(defmethod leave-node ((client html-client) (node quasiquote-node))
  (call-next-method)
  (/span (stream client))
  (decf *quasiquote-depth*))

(defmethod enter-node ((client html-client) (node unquote-node))
  (decf *quasiquote-depth*)
  (<nesting-with-depth (stream client) "quasiquote" *quasiquote-depth*)
  (call-next-method))

(defmethod leave-node ((client html-client) (node unquote-node))
  (call-next-method)
  (/span (stream client))
  (incf *quasiquote-depth*))

;;; Number

(defmethod style-class ((client html-client) (node number-node))
  "number")

;;; Symbol

(defmethod leave-node :before ((client html-client) (node interned-symbol-node))
  (when (intern? node)
    (let ((stream (stream client)))
      (<span stream "message")
      (write-string "Do not use unexported symbols." stream)
      (/span stream))))

(defmethod style-class ((client html-client) (node interned-symbol-node)) ; TODO return list and use APPEND method combination?
  (if (intern? node)
      (list* "two-package-markers" (a:ensure-list (call-next-method)))
      (call-next-method)))

(defun name-of-package? (string package)
  (or (string= string (package-name package))
      (member string (package-nicknames package) :test #'string=)))

(defmethod url ((client link-mixin) (node interned-symbol-node))
  (when (name-of-package? (package node) "CL")
    (format nil "http://l1sp.org/cl/~(~A~)" (name node))))

;;; Sequence

(defvar *nesting-depth* 0)

(defmethod enter-node ((client html-client) (node sequence-node))
  (<nesting-with-depth (stream client) "nesting" (mod *nesting-depth* 10))
  (incf *nesting-depth*)
  (call-next-method))

(defmethod leave-node ((client html-client) (node sequence-node))
  (call-next-method)
  (decf *nesting-depth*)
  (/span (stream client)))

(defmethod write-character ((client    html-client)
                            (position  t)
                            (character t)
                            (node      sequence-node))
  (let* ((start        (start node))
         (end          (end node))
         (child-start  (a:when-let ((child (first (children node))))
                         (start child)))
         (child-end    (a:when-let ((child (a:lastcar (children node))))
                         (end child)))

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

(defmethod style-class ((client html-client) (node string-node))
  "string")

;;; Vector

(defmethod style-class ((client html-client) (node vector-node))
  "vector")

;;; Cons

(defmethod style-class ((client html-client) (node cons-node))
  "cons")

;;; Errors

(defmethod enter-errors ((client html-client) (errors sequence))
  (<span (stream client) "error"))

(defmethod leave-errors ((client html-client) (errors sequence))
  (let ((stream (stream client)))
    (<span stream "message")
    (loop :for (error rest) :on errors
          :do (str stream (message error))
          :when rest
          :do (write-string "<br/>" stream))
    (/span stream)

    (/span stream)))
