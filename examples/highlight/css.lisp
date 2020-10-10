(cl:in-package #:eclector.examples.highlight)

;;; Model

(defclass property ()
  ((%name  :initarg :name
           :reader  name)
   (%value :initarg :value
           :reader  value)))

(defun make-property (name value)
  (make-instance 'property :name name :value value))

(defun make-properties (&rest names-and-values)
  (loop :for (name value) :on names-and-values :by #'cddr
        :collect (make-property name value)))

(defclass statement ()
  ((%selector   :initarg :selector
                :reader  selector)
   (%properties :initarg :properties
                :reader  properties)))

(defun make-statement (selector &rest properties)
  (let ((selector   selector)
        (properties (apply #'make-properties properties)))
    (make-instance 'statement :selector   selector
                              :properties properties)))

(defclass comment ()
  ((%content :initarg :content
             :reader  content)))

(defun make-comment (content)
  (make-instance 'comment :content content))

(defclass stylesheet ()
  ((%statements :initarg  :statements
                :type     list
                :accessor statements
                :initform '())))

(defun make-stylesheet (&rest statements)
  (make-instance 'stylesheet :statements statements))

(defun add-statements! (stylesheet &rest statements)
  (a:appendf (statements stylesheet) statements))

;;; Generator

(defmethod generate ((node property) (target stream))
  (format target "~A: ~A;~:@_" (name node) (value node)))

(defmethod generate ((node statement) (target stream))
  (let ((properties (properties node)))
    (format target "~A {~:@_~4@T" (selector node))
    (pprint-logical-block (target properties)
      (map nil (a:rcurry #'generate target) properties))
    (format target "}~@:_")))

(defmethod generate ((node comment) (target stream))
  (format target "/* ~A */~@:_" (content node)))

(defmethod generate ((node stylesheet) (target stream))
  (map nil (a:rcurry #'generate target) (statements node)))

;;; Convenience

(defun add-tooltip-statements! (stylesheet class &rest properties)
  (add-statements! stylesheet
                   (apply #'make-statement (format nil ".~A" class)
                          properties)
                   (make-statement (format nil ".~A > span" class)
                                   "display" "none")
                   (make-statement (format nil ".~A:hover > span" class)
                                   "display" "contents")))

(defun add-sequence-statements! (stylesheet count make-selector make-properties)
  (loop :for i :from 0 :below count
        :for statement = (make-instance 'statement :selector (funcall make-selector i)
                                                   :properties (funcall make-properties i))
        :do (add-statements! stylesheet statement)))

(defun add-color-sequence-statements! (stylesheet make-selector property colors)
 (add-sequence-statements!
  stylesheet (length colors) make-selector
  (lambda (i)
    (let ((color (nth i colors)))
      (list (make-property property color))))))

(let ((stream     *standard-output*)
      (stylesheet (make-instance 'stylesheet)))

  (add-statements! stylesheet (make-comment "Error tooltip"))
  (add-tooltip-statements! stylesheet "error"
                           "text-decoration" "underline red")

  (add-statements! stylesheet (make-comment "Bad whitespace tooltip"))
  (add-tooltip-statements! stylesheet "bad-whitespace"
                           "text-decoration" "orange"
                           "-webkit-text-decoration-style" "wavy"
                           "-moz-text-decoration-style" "wavy"
                           "text-decoration-style" "wavy")

  (add-statements! stylesheet (make-comment "Two package markers tooltip"))
  (add-tooltip-statements! stylesheet "interned-symbol.two-package-markers"
                           "text-decoration" "orange"
                           "-webkit-text-decoration-style" "wavy"
                           "-moz-text-decoration-style" "wavy"
                           "text-decoration-style" "wavy")

  (add-color-sequence-statements!
   stylesheet (a:curry #'format nil "quasiquote~D") "background"
   '("#ee9977" "#78db59" "#55ddff" "#dbdb59" "#ff8811" "#88cc44" "#aa22ff"
     "#db7859" "#78db59" "#5978db"))

  (add-color-sequence-statements!
   stylesheet
   (a:curry #'format nil ".nesting~D:hover > * > .syntax-open, .nesting~:*~D:hover > * > .syntax-close")
   "background-color"
   '("#ee9977" "#78db59" "#55ddff" "#dbdb59" "#ff8811" "#88cc44" "#aa22ff"
     "#db7859" "#78db59" "#5978db"))

  (fresh-line stream)
  (pprint-logical-block (stream nil)
    (generate stylesheet stream)))

(defun blend-with-white (string)
  (let* ((i (parse-integer string :start 1 :radix 16))
         (r (ldb (byte 8 16) i))
         (g (ldb (byte 8  8) i))
         (b (ldb (byte 8  0) i)))
    (format nil "~2,'0X~2,'0X~2,'0X"
            (round (a:lerp .8 r 255))
            (round (a:lerp .8 g 255))
            (round (a:lerp .8 b 255)))))
(map 'list #'blend-with-white '("#EE9977"
                                "#78DB59"
                                "#55DDFF"
                                "#DBDB59"
                                "#FF8811"
                                "#88CC44"
                                "#AA22FF"
                                "#DB7859"
                                "#78DB59"
                                "#5978DB"))

'("" "" "" "" "FFE7CF" "E7F5DA" "EED3FF" "F8E4DE"
 "E4F8DE" "DEE4F8")
