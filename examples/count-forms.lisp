(cl:defpackage #:eclector.examples.count-forms
  (:use
   #:cl))

(cl:in-package #:eclector.examples.count-forms)

(defclass form-counting-client
    (eclector.parse-result:parse-result-client)
  ((%form-count :accessor form-count
                :initform 0)))

(defmethod eclector.parse-result:make-expression-result
    ((client form-counting-client) (result t) (children t) (source t))
  (incf (form-count client)))

(defun count-forms (stream)
  (loop :with client = (make-instance 'form-counting-client)
        :for form = (eclector.parse-result:read
                     client stream nil '#1=(gensym "EOF"))
        :until (eq form '#1#)
        :finally (return (form-count client))))

(with-open-file (stream "count-forms.lisp" :direction :input)
  (count-forms stream))
