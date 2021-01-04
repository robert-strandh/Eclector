(cl:in-package #:eclector.examples.highlight)

(defclass highlight-client (eclector.parse-result:parse-result-client)
  ((%input           :initarg  :input
                     :reader   input) ; TODO single use for whitespace error detection
   ;; Environment
   (%current-package :initarg  :current-package
                     :accessor current-package)
   (%features        :initarg  :features
                     :type     list
                     :reader   features
                     :initform (map 'list (lambda (symbol)
                                            (cons (symbol-name symbol)
                                                  (package-name (symbol-package symbol))))
                                    *features*))
   ;; A list of accumulated errors.
   (%errors          :initarg  :errors
                     :type     list
                     :accessor errors
                     :initform '())))

;;; Environment

(defmethod eclector.reader:call-with-current-package ((client             highlight-client)
                                                      (thunk              t)
                                                      (package-designator t))
  (let ((old-package (current-package client)))
    (setf (current-package client) (string package-designator))
    (unwind-protect
         (funcall thunk)
      (setf (current-package client) old-package))))

;;; Symbols

(defmethod eclector.reader:interpret-symbol ((client            highlight-client)
                                             (input-stream      t)
                                             (package-indicator null)
                                             (symbol-name       t)
                                             (internp           t))
  (make-instance 'cst:uninterned-symbol-node :name symbol-name))

(defmethod eclector.reader:interpret-symbol ((client            highlight-client)
                                             (input-stream      t)
                                             (package-indicator (eql :keyword))
                                             (symbol-name       t)
                                             (internp           t))
  (make-instance 'cst:keyword-symbol-node :name symbol-name))

(defun name-of-package-p (string package)
  (or (string= string (package-name package))
      (member string (package-nicknames package) :test #'string=)))

(defmethod eclector.reader:interpret-symbol ((client            highlight-client)
                                             (input-stream      t)
                                             (package-indicator t)
                                             (symbol-name       t)
                                             (internp           t))
  (let* ((current-package-name (current-package client))
         (current-package (find-package current-package-name))
         (keyword (find-package '#:keyword))
         (cl (find-package '#:cl)))
    (flet ((standard-symbol-class (name)
             (cond ((find name lambda-list-keywords :test #'string=)
                    'cst:lambda-list-keyword-symbol-node)
                   ((not (eq :external (nth-value 1 (find-symbol symbol-name cl))))
                    (eclector.reader::%recoverable-reader-error
                     input-stream 'eclector.reader::symbol-does-not-exist
                     :package cl :symbol-name symbol-name
                     :report 'eclector.reader::inject-nil)
                    'cst:standard-symbol-node)
                   (t
                    'cst:standard-symbol-node))))
      (multiple-value-bind (class package internp)
          (cond ((not (eq package-indicator :current))
                 (values (cond ((name-of-package-p package-indicator keyword)
                                'cst:keyword-symbol-node)
                               ((name-of-package-p package-indicator cl)
                                (standard-symbol-class symbol-name))
                               (t
                                'cst:interned-symbol-node))
                         package-indicator internp))
                ((and current-package (eq current-package keyword))
                 'cst:keyword-symbol-node)
                ((and current-package
                      (eq :external (nth-value 1 (find-symbol symbol-name current-package))))
                 (values 'cst:interned-symbol-node current-package-name nil))
                ((eq :external (nth-value 1 (find-symbol symbol-name cl)))
                 (values (standard-symbol-class symbol-name) "CL" nil))
                (t
                 #+no (warn "No idea what's going on with ~S in ~S"
                            symbol-name current-package-name)
                 (values 'cst:interned-symbol-node current-package-name nil)))
        (apply #'make-instance class :name symbol-name
               (when package
                 (list :package package :internp internp)))))))

(defmethod eclector.reader::valid-symbol-p ((client  highlight-client)
                                            (symbol  cst:symbol-node)
                                            (purpose t))
  t)

(defmethod eclector.reader:make-structure-instance
    ((client highlight-client) (name t) (initargs t))
  (make-instance 'cst:structure-node))

(defmethod eclector.reader:check-feature-expression
    ((client highlight-client) (feature-expression cst:symbol-node))
  t)

(defmethod eclector.reader:evaluate-feature-expression
    ((client highlight-client) (feature-expression cst:symbol-node))
  (find-if (lambda (feature)
             (destructuring-bind (symbol-name . package-name) feature
               (and (string= symbol-name (cst:name feature-expression))
                    (string= package-name (cst:package feature-expression)))))
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
               (cst:keyword-symbol-node
                (or (find-symbol (cst:name expression) '#:keyword)
                    (give-up)))
               (cst:interned-symbol-node
                (if (string= (cst:package expression) "CL")
                    (multiple-value-bind (symbol kind)
                        (find-symbol (cst:name expression) '#:cl)
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
  (make-instance 'cst:quote-node :object material))

(defmethod eclector.reader:wrap-in-quasiquote ((client highlight-client)
                                               (form   t))
  (make-instance 'cst:quasiquote-node :object form))

(defmethod eclector.reader:wrap-in-unquote ((client highlight-client)
                                            (form   t))
  (make-instance 'cst:unquote-node :object form))

(defmethod eclector.reader:wrap-in-unquote-splicing ((client highlight-client)
                                                     (form   t))
  (make-instance 'cst:unquote-node :object form))

(defmethod eclector.reader:wrap-in-function ((client highlight-client)
                                             (name   t))
  (make-instance 'cst:function-node :object name))

;;; Generic conversion from results to nodes

(defmethod eclector.parse-result:make-skipped-input-result
    ((client highlight-client) (stream t) (reason t) (source t))
  (let ((class (typecase reason
                 ((eql :block-comment)       'cst:block-comment-node)
                 ((cons (eql :line-comment)) 'cst:line-comment-node)
                 (t                          'cst:skipped-node))))
    (make-instance class :children '()
                         :source   source)))

(defmethod eclector.parse-result:make-expression-result
    ((client highlight-client) (result cst:node) (children t) (source t))
  ;; This is used if RESULT already is a `cst:node'.
  (reinitialize-instance result :children children :source source))

(defmethod eclector.parse-result:make-expression-result
    ((client highlight-client) (result t) (children t) (source t))
  (let ((class (cond ((and (a:lastcar children)
                           (eq result (cst:object (a:lastcar children))))
                      'cst:feature-expression-node)
                     (t (result-node-class client result)))))
    (make-instance class :object   result
                         :children children
                         :source   source)))

(defmethod result-node-class ((client highlight-client) (result number))
  'cst:number-node)

(defmethod result-node-class ((client highlight-client) (result character))
  'cst:character-node)

(defmethod result-node-class ((client highlight-client) (result symbol))
  'cst:symbol-node)

(defmethod result-node-class ((client highlight-client) (result array))
  'cst:array-node)

(defmethod result-node-class ((client highlight-client) (result string))
  'cst:string-node)

(defmethod result-node-class ((client highlight-client) (result vector))
  'cst:vector-node)

(defmethod result-node-class ((client highlight-client) (result cons))
  'cst:cons-node)

(defmethod result-node-class ((client highlight-client) (result pathname))
  'cst:pathname-node)

;;;

(defun read-stuff (input &key package
                              (client (make-instance 'highlight-client
                                                     :input input
                                                     :current-package package)))
  (let ((stream (make-string-input-stream input)))
    (eclector.reader:call-as-top-level-read
     client
     (lambda ()
       (handler-bind ((error (lambda (condition)
                               (let ((position (file-position stream)))
                                 (push (cst:make-syntax-error
                                        (1- position) position (princ-to-string condition))
                                       (errors client)))
                               (eclector.reader:recover))))
         (loop :for (object kind result)
                  = (multiple-value-list
                     (eclector.reader:read-maybe-nothing client stream nil nil))
               :until (eq kind :eof)
               :unless (eq kind :whitespace)
               :collect result :into results
               :finally (return (values (cst:make-cst results) (errors client))))))
     stream nil :eof t)))
