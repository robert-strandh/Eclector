(cl:in-package #:eclector.examples.highlight.cst)

;;; `node'
;;;
;;; Superclass for syntax tree nodes.

(defclass node ()
  ((%source :initarg  :source
            :type     (cons a:non-negative-integer a:non-negative-integer)
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
            :initform nil)))

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

;;; `root-node'
;;;
;;; Root node of the concrete syntax tree.

(defclass root-node (children-mixin node)
  ())

(defun make-cst (children)
  (make-instance 'root-node :children children
                            :source   (cons 0 100000000000000))) ; TODO

;;; Node class for recorded syntax errors

(defclass syntax-error (node)
  ((%message :initarg :message
             :reader  message)))

(defun make-syntax-error (start end message)
  (make-instance 'syntax-error :source  (cons start end)
                               :message message))

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

(defclass character-node (object-node-mixin
                          inner-node)
  ())

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
   (%internp :initarg :internp
             :reader  internp))
  (:default-initargs
   :package (a:required-argument :package)
   :internp (a:required-argument :internp)))

(defmethod print-object ((object interned-symbol-node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A ~D-~D"
            (package object) (name object) (start object) (end object))))

(defclass standard-symbol-node (interned-symbol-node)
  ())

(defclass lambda-list-keyword-symbol-node (standard-symbol-node)
  ())

;;; Structure literal

(defclass structure-node (inner-node)
  ())

;;; Array

(defclass array-node (object-node-mixin
                      inner-node)
  ())

;;; Sequence

(defclass sequence-node (object-node-mixin
                         inner-node)
  ())

(defclass string-node (sequence-node) ())

(defclass vector-node (sequence-node
                       array-node)
  ())

;;; Cons

(defclass cons-node (sequence-node) ())

;;; Pathname

(defclass pathname-node (object-node-mixin
                         inner-node)
  ())

