(cl:in-package #:eclector.examples.highlight.cst)

;;; Concrete syntax tree protocol

(defgeneric source (node))

(defgeneric start (node))

(defgeneric end (node))

(defgeneric parent (node))

(defgeneric children (node))

(defgeneric object (node))

;;; Default behavior

(defmethod parent ((node t))
  nil)

(defmethod children ((node t))
  '())

(defmethod object ((node t))
  nil)
