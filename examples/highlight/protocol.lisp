(cl:in-package #:eclector.examples.highlight)

;;; CST protocol

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

;;;

(defgeneric result-node-class (result)
  )
