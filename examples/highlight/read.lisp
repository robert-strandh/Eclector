(cl:in-package #:eclector.examples.highlight)

(defclass highlight-client (eclector.parse-result:parse-result-client)
  ((%input           :initarg  :input
                     :reader   input) ; TODO single use for whitespace error detection
   ;; Environment
   (%current-package :initarg  :current-package
                     :accessor current-package)
   (%features        :initarg  :features
                     :reader   features
                     :initform (map 'list (lambda (symbol)
                                            (cons (symbol-name symbol)
                                                  (package-name (symbol-package symbol))))
                                    *features*))
   ;;
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

(defmethod eclector.reader:interpret-symbol ((client            highlight-client)
                                             (input-stream      t)
                                             (package-indicator null)
                                             (symbol-name       t)
                                             (internp           t))
  (make-instance 'uninterned-symbol-node :name symbol-name))

(defmethod eclector.reader:interpret-symbol ((client            highlight-client)
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
               #+no (warn "No idea what's going on with ~S in ~S"
                     symbol-name current-package-name)
               (values 'interned-symbol-node current-package-name nil)))
      (apply #'make-instance class :name symbol-name
             (when package
               (list :package package :intern? intern?))))))

(defmethod eclector.reader:check-feature-expression
    ((client highlight-client) (feature-expression symbol-node))
  t)

(defmethod eclector.reader:evaluate-feature-expression
    ((client highlight-client) (feature-expression symbol-node))
  (find-if (lambda (feature)
             (destructuring-bind (symbol-name . package-name) feature
               (and (string= symbol-name (name feature-expression))
                    (string= package-name (package feature-expression)))))
           (features client)))

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

;;; Creating S-expressions

(defmethod eclector.reader:wrap-in-quote ((client   highlight-client)
                                          (material t))
  (make-instance 'quote-node :object material))

(defmethod eclector.reader:wrap-in-quasiquote ((client highlight-client)
                                               (form   t))
  (make-instance 'quasiquote-node :object form))

(defmethod eclector.reader:wrap-in-unquote ((client highlight-client)
                                            (form   t))
  (make-instance 'unquote-node :object form))

(defmethod eclector.reader:wrap-in-unquote-splicing ((client highlight-client)
                                                     (form   t))
  (make-instance 'unquote-node :object form))

(defmethod eclector.reader:wrap-in-function ((client highlight-client)
                                             (name   t))
  (make-instance 'function-node :object name))

;;; Nodes from results

(defmethod eclector.parse-result:make-skipped-input-result
    ((client highlight-client) (stream t) (reason t) (source t))
  (let ((class (typecase reason
                 ((eql :block-comment)       'block-comment-node)
                 ((cons (eql :line-comment)) 'line-comment-node)
                 (t                          'skipped-node))))
    (make-instance class :children '()
                         :source   source)))

(defmethod eclector.parse-result:make-expression-result
    ((client highlight-client) (result node) (children t) (source t))
  ;; This is used if RESULT already is a `node'.
  (reinitialize-instance result :children children :source source))

(defmethod eclector.parse-result:make-expression-result
    ((client highlight-client) (result t) (children t) (source t))
  (let ((class (cond ((and (a:lastcar children)
                           (eq result (object (a:lastcar children))))
                      'feature-expression-node)
                     (t (result-node-class result)))))
    (make-instance class :object   result
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
                               (let ((position (file-position stream)))
                                 (push (make-instance 'syntax-error :source  (cons (1- position) position)
                                                                    :message (princ-to-string condition))
                                       (errors client)))
                               (eclector.reader:recover))))
         (loop :for (object kind result)
                  = (multiple-value-list
                     (eclector.reader:read-maybe-nothing client stream nil nil))
               :until (eq kind :eof)
               :unless (eq kind :whitespace)
               :collect result :into results
               :finally (return (values (make-instance 'cst :children results
                                                            :source   (cons 0 100000000000000)) ; TODO
                                        (errors client))))))
     stream nil :eof t)))
