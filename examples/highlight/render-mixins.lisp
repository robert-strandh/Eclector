(cl:in-package #:eclector.examples.highlight)

;;; `nesting-tracking-mixin'

(defclass nesting-tracking-mixin ()
  ((%block-comment-depth :initarg  :block-comment-depth
                         :accessor block-comment-depth
                         :initform 0)
   (%quasiquote-depth    :initarg  :quasiquote-depth
                         :accessor quasiquote-depth
                         :initform 0)
   (%nesting-depth       :initarg  :nesting-depth
                         :accessor nesting-depth
                         :initform 0)))

;;; Block comment

(defmethod enter-node :before ((client nesting-tracking-mixin)
                               (node   block-comment-node))
  (incf (block-comment-depth client)))

(defmethod leave-node :after ((client nesting-tracking-mixin)
                              (node   block-comment-node))
  (decf (block-comment-depth client)))

;;; Quasiquote

(defmethod enter-node :before ((client nesting-tracking-mixin)
                               (node   quasiquote-node))
  (incf (quasiquote-depth client)))

(defmethod leave-node :after ((client nesting-tracking-mixin)
                              (node   quasiquote-node))
  (decf (quasiquote-depth client)))

(defmethod enter-node :before ((client nesting-tracking-mixin)
                               (node   unquote-node))
  (decf (quasiquote-depth client)))

(defmethod leave-node :after ((client nesting-tracking-mixin)
                              (node   unquote-node))
  (incf (quasiquote-depth client)))

;;; Sequence

(defmethod enter-node :before ((client nesting-tracking-mixin)
                               (node   sequence-node))
  (incf (nesting-depth client)))

(defmethod leave-node :after ((client nesting-tracking-mixin)
                              (node   sequence-node))
  (decf (nesting-depth client)))
