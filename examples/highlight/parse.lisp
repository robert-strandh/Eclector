(cl:in-package #:eclector.examples.highlight)

(defclass highlight-client (eclector.parse-result:parse-result-client)
  ((%input           :initarg  :input
                     :reader   input)
   (%current-package :initarg  :current-package
                     :accessor current-package)
   (%errors          :initarg  :errors
                     :accessor errors
                     :initform '())))

;;; Nodes instead of objects
(defmethod eclector.reader:call-with-current-package ((client             highlight-client)
                                                      (thunk              t)
                                                      (package-designator t))
  (let ((old-package (current-package client)))
    (setf (current-package client) (string package-designator))
    (unwind-protect
         (funcall thunk)
      (setf (current-package client) old-package))))

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
  (let* ((current-package-name (current-package client))
         (current-package      (find-package current-package-name)))
    (multiple-value-bind (class package intern?)
        (cond ((not (eq package-indicator :current))
               (values 'interned-symbol-node package-indicator internp))
              ((and current-package (eq current-package (find-package '#:keyword)))
               'keyword-symbol-node)
              ((and current-package
                    (eq :external (nth-value 1 (find-symbol symbol-name current-package))))
               (values 'interned-symbol-node current-package-name nil))
              ((eq :external (nth-value 1 (find-symbol symbol-name (find-package '#:CL))))
               (let ((class (if (find symbol-name lambda-list-keywords
                                      :test #'string=)
                                'lambda-list-keyword-symbol-node
                                'interned-symbol-node)))
                (values class "CL" nil)))
              (t
               (warn "No idea what's going on with ~S in ~S"
                     symbol-name current-package-name)
               (values 'interned-symbol-node current-package-name nil)))
      (apply #'make-instance class :name symbol-name
             (when package
               (list :package package :intern? intern?))))))

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

(defmethod eclector.reader:evaluate-expression
    ((client highlight-client) (expression t))
  (labels ((give-up ()
             (return-from eclector.reader:evaluate-expression
               expression))
           (evaluate (expression)
             (typecase expression
               (cons
                (destructuring-bind (head &rest rest)
                    (map 'list #'evaluate expression)
                  (unless (member head '(list list* + - * /))
                    (give-up))
                  (apply head rest)))
               (keyword-symbol-node
                (or (find-symbol (name expression) '#:keyword)
                    (give-up)))
               (interned-symbol-node
                (if (string= (package expression) "CL")
                    (multiple-value-bind (symbol kind)
                        (find-symbol (name expression) '#:cl)
                      (if kind
                          symbol
                          (give-up)))
                    (give-up)))
               (t
                expression))))
    (evaluate expression)))

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

(defun read-stuff (input &key package)
  (let ((client (make-instance 'highlight-client :input input :current-package package))
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

(defun highlight (input-string &key (package "COMMON-LISP-USER")
                                    (client (make-instance 'minimal-html-client :stream *standard-output*)))
  (multiple-value-bind (cst errors) (read-stuff input-string :package package)
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

(defun highlight-string (string &key package client)
  (with-output-to-string (stream)
    (apply #'highlight string stream (append (when package
                                               (list :package package))
                                             (when client
                                               (list :client client))))))

(defun process (input output &key (client (make-instance 'linking-html-client :input input :stream *standard-output*)))
  (a:with-output-to-file (stream output :if-exists :supersede)
    (reinitialize-instance client :stream stream)
    (apply #'highlight input (when client
                               (list :client client)))))
