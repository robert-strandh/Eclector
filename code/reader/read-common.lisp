(cl:in-package #:eclector.reader)

;;; We have provide our own PEEK-CHAR function because CL:PEEK-CHAR
;;; obviously does not use Eclector's readtable.

(defun peek-char (&optional peek-type
                            (input-stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  (flet ((done (value)
           (cond ((not (eq value '#1=#.(gensym "EOF")))
                  (return-from peek-char value))
                 (eof-error-p
                  (%reader-error input-stream 'end-of-file))
                 (t
                  (return-from peek-char eof-value)))))
    (if (not (eq peek-type t))
        (done (cl:peek-char peek-type input-stream nil '#1# recursive-p))
        (loop with readtable = *readtable*
              for char = (cl:peek-char nil input-stream nil '#1# recursive-p)
              while (and (not (eq char '#1#))
                         (eq (eclector.readtable:syntax-type readtable char)
                             :whitespace))
              do (read-char input-stream) ; consume whitespace char
              finally (done char)))))

;;;

(defmethod call-reader-macro (client input-stream char readtable)
  (let ((function (eclector.readtable:get-macro-character readtable char)))
    (funcall function input-stream char)))

(defmethod read-maybe-nothing (client input-stream eof-error-p eof-value)
  (let ((*skip-reason* nil)
        (char (read-char input-stream eof-error-p)))
    (if (null char)
        (values eof-value :eof)
        (case (eclector.readtable:syntax-type *readtable* char)
          (:whitespace
           (values nil :whitespace))
          ((:terminating-macro :non-terminating-macro)
           (let ((values (multiple-value-list
                          (call-reader-macro
                           client input-stream char *readtable*))))
             (cond
               ((null values)
                (note-skipped-input client input-stream
                                    (or *skip-reason* :reader-macro))
                (values nil :skip))
               ;; This case takes care of reader macro not returning
               ;; nil when *READ-SUPPRESS* is true.
               (*read-suppress*
                (note-skipped-input client input-stream
                                    (or *skip-reason* '*read-suppress*))
                (values nil :suppress))
               (t
                (values (car values) :value)))))
          (t
           (unread-char char input-stream)
           (values (read-token client input-stream eof-error-p eof-value)
                   :value))))))

(defmethod read-common (client input-stream eof-error-p eof-value)
  (tagbody
   :start
     (multiple-value-bind (value what)
         (read-maybe-nothing client input-stream eof-error-p eof-value)
       (ecase what
         ((:eof :suppress :value)
          (return-from read-common value))
         ((:whitespace :skip)
          (go :start))))))

(defmethod read-common :around (client input-stream eof-error-p eof-value)
  (let ((*input-stream* input-stream))
    (call-next-method)))
