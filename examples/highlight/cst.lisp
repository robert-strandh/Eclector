(cl:in-package #:eclector.examples.highlight)

(defclass node ()
  ((%value    :initarg  :value
              :reader   value)
   (%parent   :initarg  :parent
              :reader   parent
              :writer   (setf %parent)
              :initform nil)
   (%children :initarg  :children
              :reader   children)
   (%source   :initarg  :source
              :reader   source)))

(defmethod shared-initialize :after ((instance   node)
                                     (slot-names t)
                                     &key (children nil children-supplied?))
  (when children-supplied?
    (map nil (lambda (child) (setf (%parent child) instance)) children)))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D-~D" (start object) (end object))))

(defmethod start ((node node))
  (car (source node)))

(defmethod end ((node node))
  (cdr (source node)))

;;; `cst'
;;;
;;; Root node of the concrete syntax tree.

(defclass cst (node) ())

;;;

(defclass syntax-error (node)
  ((%message :initarg :message
             :reader  message)))

;;; Skipped

(defclass skipped-node (node) ())

(defclass comment-node (skipped-node) ())

(defclass block-comment-node (comment-node) ())

(defclass line-comment-node (comment-node) ())

;;; S-expression creation

(defclass quote-node (node) ())

(defclass quasiquote-node (node) ())

(defclass unquote-node (node) ())

(defclass function-node (node) ())

;;; Feature expression

(defclass feature-expression-node (node) ())

;;; Number

(defclass number-node (node) ())

(defmethod result-node-class ((result number))
  'number-node)

(defclass character-node (node) ())

(defmethod result-node-class ((result character))
  'character-node)

;;; Symbol

(defclass symbol-node (node) ; TODO could be atom
  ((%name :initarg :name
          :type    string
          :reader  name))
  (:default-initargs :value nil)) ; TODO the value slot is almost unused

(defmethod print-object ((object symbol-node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~D-~D"
            (name object) (start object) (end object))))

(defclass uninterned-symbol-node (symbol-node)
  ())

(defclass keyword-symbol-node (symbol-node)
  ())

(defclass interned-symbol-node (symbol-node)
  ((%package :initarg :package
             :type    string
             :reader  package)
   (%intern? :initarg :intern?
             :reader  intern?)))

(defmethod print-object ((object interned-symbol-node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A ~D-~D"
            (package object) (name object) (start object) (end object))))

(defclass lambda-list-keyword-symbol-node (interned-symbol-node)
  ())

(defmethod result-node-class ((result symbol))
  'symbol-node)

;;; Array

(defclass array-node (node) ())

(defmethod result-node-class ((result array))
  'array-node)

;;; Sequence

(defclass sequence-node (node) ())

(defclass string-node (sequence-node) ())

(defmethod result-node-class ((result string))
  'string-node)

(defclass vector-node (sequence-node array-node) ())

(defmethod result-node-class ((result vector))
  'vector-node)

;;; Cons

(defclass cons-node (sequence-node) ())

(defmethod result-node-class ((result cons))
  'cons-node)

;;; Pathname

(defclass pathname-node (node) ())

(defmethod result-node-class ((result pathname))
  'pathname-node)
