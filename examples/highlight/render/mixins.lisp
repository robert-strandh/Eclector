(cl:in-package #:eclector.examples.highlight.render)

;;; `stream-mixin'

(defclass stream-mixin ()
  ((%stream :initarg :stream
            :reader  stream))
  (:default-initargs
   :stream (a:required-argument :stream)))

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
                               (node   cst::block-comment-node))
  (incf (block-comment-depth client)))

(defmethod leave-node :after ((client nesting-tracking-mixin)
                              (node   cst::block-comment-node))
  (decf (block-comment-depth client)))

;;; Quasiquote

(defmethod enter-node :before ((client nesting-tracking-mixin)
                               (node   cst::quasiquote-node))
  (incf (quasiquote-depth client)))

(defmethod leave-node :after ((client nesting-tracking-mixin)
                              (node   cst::quasiquote-node))
  (decf (quasiquote-depth client)))

(defmethod enter-node :before ((client nesting-tracking-mixin)
                               (node   cst::unquote-node))
  (decf (quasiquote-depth client)))

(defmethod leave-node :after ((client nesting-tracking-mixin)
                              (node   cst::unquote-node))
  (incf (quasiquote-depth client)))

;;; Sequence

(defmethod enter-node :before ((client nesting-tracking-mixin)
                               (node   cst::sequence-node))
  (incf (nesting-depth client)))

(defmethod leave-node :after ((client nesting-tracking-mixin)
                              (node   cst::sequence-node))
  (decf (nesting-depth client)))
