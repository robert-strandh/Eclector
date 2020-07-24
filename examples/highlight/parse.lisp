(cl:in-package #:eclector.examples.highlight)

(defclass highlight-client (eclector.parse-result:parse-result-client)
  ((%input  :initarg  :input
            :reader   input)
   (%errors :initarg  :errors
            :accessor errors
            :initform '())))

;;; Nodes instead of objects

(defmethod eclector.reader:interpret-symbol ((client highlight-client)
                                             (input-stream      t)
                                             (package-indicator null)
                                             (symbol-name       t)
                                             (internp           t))
  (make-instance 'uninterned-symbol-node :name symbol-name))

(defmethod eclector.reader:interpret-symbol ((client highlight-client)
                                             (input-stream      t)
                                             (package-indicator (eql :keyword))
                                             (symbol-name       t)
                                             (internp           t))
  (make-instance 'keyword-symbol-node :name symbol-name))

(defmethod eclector.reader:interpret-symbol ((client            highlight-client)
                                             (input-stream      t)
                                             (package-indicator t)
                                             (symbol-name       t)
                                             (internp           t))
  (multiple-value-bind (package intern?)
      (cond ((not (eq package-indicator :current))
             (values package-indicator internp))
            ((find-symbol symbol-name (find-package '#:CL))
             (values "CL" nil))
            (t
             (values "foo" nil)))
    (make-instance 'interned-symbol-node :package package
                                         :name    symbol-name
                                         :intern? intern?)))

(defmethod eclector.reader:wrap-in-quote ((client   highlight-client)
                                          (material t))
  (make-instance 'quote-node :value material))

(defmethod eclector.reader:wrap-in-quasiquote ((client highlight-client)
                                               (form   t))
  (make-instance 'quasiquote-node :value form))

(defmethod eclector.reader:wrap-in-unquote ((client highlight-client)
                                            (form   t))
  (make-instance 'unquote-node :value form))

(defmethod eclector.reader:wrap-in-unquote-splicing ((client highlight-client)
                                                     (form   t))
  (make-instance 'unquote-node :value form))

(defmethod eclector.reader:wrap-in-function ((client highlight-client)
                                             (name   t))
  (make-instance 'function-node :value name))

(defmethod eclector.reader:evaluate-feature-expression
    ((client highlight-client) (feature-expression t))
  (and (typep feature-expression 'symbol-node)
       (search "TRUE" (name feature-expression))))

;; TODO read-time-evaluation

;;; Nodes from results

(defmethod eclector.parse-result:make-skipped-input-result
    ((client highlight-client) (stream t) (reason t) (source t))
  (let ((class (typecase reason
                 ((cons (eql :block-comment)) 'block-comment-node)
                 ((cons (eql :line-comment))  'line-comment-node)
                 (t                           'skipped-node))))
    (make-instance class :value    nil
                         :children '()
                         :source   source)))

(defmethod eclector.parse-result:make-expression-result
    ((client highlight-client) (result node) (children t) (source t))
  ;; This is used if RESULT already is a `node'.
  (reinitialize-instance result :children children :source source))

(defmethod eclector.parse-result:make-expression-result
    ((client highlight-client) (result t) (children t) (source t))
  (let ((class (cond ((and (a:lastcar children)
                           (eq result (value (a:lastcar children))))
                      'feature-expression-node)
                     (t (result-node-class result)))))
    (make-instance class :value    result
                         :children children
                         :source   source)))

;;;

(defun read-stuff (input)
  (let ((client (make-instance 'highlight-client :input input))
        (stream (make-string-input-stream input)))
    (eclector.reader:call-as-top-level-read
     client
     (lambda ()
      (handler-bind ((error (lambda (condition)
                              (push (make-instance 'syntax-error :source  (cons (1- (file-position stream))
                                                                                (file-position stream))
                                                                 :message (princ-to-string condition))
                                    (errors client))
                              (eclector.reader:recover))))
        (loop :for (object kind result)
                = (multiple-value-list
                   (eclector.reader:read-maybe-nothing client stream nil nil))
              :until (eq kind :eof)
              :unless (eq kind :whitespace)
                :collect result :into results
              :finally (return (values (make-instance 'cst :value    nil ; TODO make a node without value. and without parent?
                                                           :children results
                                                           :source   (cons 0 100000000000000))
                                       (errors client))))))
     stream nil :eof t)))

(defun highlight (input-string &key (client (make-instance 'minimal-html-client :stream *standard-output*)))
  (multiple-value-bind (cst errors) (read-stuff input-string)
    (let ((node cst)
          (stack (list cst)))
      (flet ((maybe-end-errors (position)
               (a:when-let ((errors (remove position errors
                                            :test-not #'eql :key #'end)))
                 (leave-errors client errors)))
             (maybe-start-errors (position)
               (a:when-let ((errors (remove position errors
                                            :test-not #'eql :key #'start)))
                 (enter-errors client errors)))
             (maybe-leave-nodes (position)
               (loop :while (eql position (end node))
                     :do (leave-node client node)
                         (pop stack)
                         (setf node (parent node))))
             (maybe-enter-node (position)
               (a:when-let ((child (find position (children node) :key #'start)))
                 (enter-node client child)
                 (push child stack)
                 (setf node child))))
        (enter-node client cst)
        (loop :for character :across input-string
              :for position :from 0

              :do (maybe-end-errors position)
                  (maybe-leave-nodes position)

                  (maybe-enter-node position)
                  (maybe-start-errors position)

              :do (write-character client position character node)

              :finally (let ((end (length input-string)))
                         (when (and (eql character #\Newline)
                                    (find end errors :test #'eql :key #'end))
                           (write-char #\¶ (stream client)))
                         (maybe-end-errors end)

                         (map nil (lambda (node)
                                    (leave-node client node))
                              stack)))))))

(defun highlight-string (string &key client)
  (with-output-to-string (stream)
    (apply #'highlight string stream (when client
                                       (list :client client)))))

(defun process (input output &key (client (make-instance 'linking-html-client :input input :stream *standard-output*)))
  (a:with-output-to-file (stream output :if-exists :supersede)
    (reinitialize-instance client :stream stream)
    (apply #'highlight input (when client
                               (list :client client)))))
