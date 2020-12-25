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

;;; Client protocol

(defgeneric write-character (client position character node)
  (:documentation
   ""))

(defgeneric enter-node (client node)
  (:documentation
   ""))

(defgeneric leave-node (client node)
  (:documentation
   ""))

(defgeneric enter-errors (client errors)
  (:documentation
   ""))

(defgeneric leave-errors (client errors)
  (:documentation
   ""))

;;;

(defgeneric style-class (client node)
  (:documentation
   ""))

(defgeneric url (client node)
  (:documentation
   ""))
