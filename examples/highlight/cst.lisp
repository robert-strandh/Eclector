(cl:in-package #:eclector.examples.highlight)

;;; `node'
;;;
;;; Superclass for syntax tree nodes.

(defclass node ()
  ((%source :initarg  :source
            :reader   source)))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D-~D" (start object) (end object))))

(defmethod start ((node node))
  (car (source node)))

(defmethod end ((node node))
  (cdr (source node)))

;;; Inner and leaf nodes

(defclass object-node-mixin ()
  ((%object :initarg  :object
            :reader   object)))

(defclass parent-mixin ()
  ((%parent :initarg  :parent
            :reader   parent
            :writer   (setf %parent)
            :initform nil)
   (%children :initarg  :children
              :reader   children)))

(defclass children-mixin ()
  ((%children :initarg  :children
              :reader   children)))

(defmethod shared-initialize :after ((instance   children-mixin)
                                     (slot-names t)
                                     &key (children nil children-supplied?))
  (when children-supplied?
    (map nil (lambda (child) (setf (%parent child) instance)) children)))

(defclass leaf-node (parent-mixin
                     node)
  ())

(defclass inner-node (parent-mixin
                      children-mixin
                      node)
  ())

;;; `cst'
;;;
;;; Root node of the concrete syntax tree.

(defclass cst (children-mixin node)
  ())

;;;

(defclass syntax-error (node)
  ((%message :initarg :message
             :reader  message)))

;;; Skipped

(defclass skipped-node (inner-node) ())

(defclass comment-node (skipped-node) ())

(defclass line-comment-node (comment-node) ()) ; TODO this cannot have children

(defclass block-comment-node (comment-node) ())

;;; S-expression creation

(defclass quote-node (object-node-mixin
                      inner-node)
  ())

(defclass quasiquote-node (object-node-mixin
                           inner-node)
  ())

(defclass unquote-node (object-node-mixin
                        inner-node)
  ())

(defclass function-node (object-node-mixin
                         inner-node)
  ())

;;; Feature expression

(defclass feature-expression-node (object-node-mixin
                                   inner-node)
  ())

;;; Number

(defclass number-node (object-node-mixin
                       inner-node)
  ())

(defmethod result-node-class ((result number))
  'number-node)

(defclass character-node (object-node-mixin
                          inner-node)
  ())

(defmethod result-node-class ((result character))
  'character-node)

;;; Symbol

(defclass symbol-node (inner-node) ; TODO could be atom
  ((%name :initarg :name
          :type    string
          :reader  name)))

(defmethod shared-initialize :after ((instance symbol-node) (slot-names t)
                                     &key object)
  (declare (ignore object)))

(defmethod print-object ((object symbol-node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~D-~D"
            (name object) (start object) (end object))))

(defclass uninterned-symbol-node (symbol-node)
  ())

(defclass keyword-symbol-node (symbol-node)
  ())

(defmethod package ((node keyword-symbol-node))
  "KEYWORD")

(defclass interned-symbol-node (symbol-node)
  ((%package :initarg :package
             :type    string
             :reader  package)
   (%intern? :initarg :intern?
             :reader  intern?))
  (:default-initargs
   :package (a:required-argument :package)
   :intern? (a:required-argument :intern?)))

(defmethod print-object ((object interned-symbol-node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A ~D-~D"
            (package object) (name object) (start object) (end object))))

(defclass lambda-list-keyword-symbol-node (interned-symbol-node)
  ())

(defmethod result-node-class ((result symbol))
  'symbol-node)

;;; Array

(defclass array-node (object-node-mixin
                      inner-node)
  ())

(defmethod result-node-class ((result array))
  'array-node)

;;; Sequence

(defclass sequence-node (object-node-mixin
                         inner-node)
  ())

(defclass string-node (sequence-node) ())

(defmethod result-node-class ((result string))
  'string-node)

(defclass vector-node (sequence-node
                       array-node)
  ())

(defmethod result-node-class ((result vector))
  'vector-node)

;;; Cons

(defclass cons-node (sequence-node) ())

(defmethod result-node-class ((result cons))
  'cons-node)

;;; Pathname

(defclass pathname-node (object-node-mixin
                         inner-node)
  ())

(defmethod result-node-class ((result pathname))
  'pathname-node)
