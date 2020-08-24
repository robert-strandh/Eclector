(cl:defpackage #:eclector.examples.read-maybe-nothing
  (:use
   #:cl))

(cl:in-package #:eclector.examples.read-maybe-nothing)

(with-input-from-string (stream "foo bar (1 #1=2 #1#) \"whoo\" ")
  (eclector.reader:call-as-top-level-read
   nil
   (lambda ()
     (loop :for (result kind) = (multiple-value-list
                                 (eclector.reader:read-maybe-nothing
                                  nil stream nil :eof))
           :until (eq kind :eof)
           :collect (cons result kind)))
   stream nil nil nil))

(defclass client (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client client) (result t) (children t) (source t))
  (list :result result :children children :source source))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client client) (stream t) (reason t) (source t))
  (list :reason reason :source source))

(let ((stream (make-string-input-stream "; comment
foo #|fez|#
 bar    (1 #1=2 #1#) \"whoo\" "))
      (client (make-instance 'client)))
  (eclector.reader:call-as-top-level-read
   client
   (lambda ()
     (loop :for (result kind parse-result)
              = (multiple-value-list
                 (eclector.reader:read-maybe-nothing
                  client stream nil :eof))
           :until (eq kind :eof)
           :collect (list result kind parse-result)))
   stream nil nil nil))

(let ((stream (make-string-input-stream "; comment
foo #|fez|# #1=5 #1#
 bar    (1 #1# #1#) \"whoo\" ]"))
      (client (make-instance 'client)))
  (eclector.reader:call-as-top-level-read
   client
   (lambda ()
     (eclector.reader:read-delimited-list #\] stream t))
   stream nil nil nil))

(let ((stream (make-string-input-stream "; comment
foo #|fez|# #1=5 #1#
 bar    (1 #1# #1#) \"whoo\" ]"))
      (eclector.reader:*client* (make-instance 'client)))
  (eclector.reader:read-delimited-list #\] stream))
