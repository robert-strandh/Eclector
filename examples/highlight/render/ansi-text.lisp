(cl:in-package #:eclector.examples.highlight.render)

;;; Style

(defclass style ()
  ((%foreground :initarg :foreground
                :reader  foreground)
   (%background :initarg :background
                :reader  background)
   (%underlinep :initarg :underlinep
                :reader  underlinep)))

(defmethod print-object ((object style) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~A ~A~:[~; underline~]"
            (foreground object)
            (background object)
            (underlinep object))))

(defun make-style (foreground background underlinep)
  (make-instance 'style :foreground foreground
                        :background background
                        :underlinep underlinep))

(defun change-style (current &key foreground
                                  background
                                  (underlinep nil underlinep-suppliedp))
  (let* ((current-foreground (foreground current))
         (current-background (background current))
         (current-underline  (underlinep current))
         (new-foreground     (or foreground
                                 current-foreground))
         (new-background     (or background
                                 current-background))
         (new-underline      (if underlinep-suppliedp
                                 underlinep
                                 current-underline)))
    (if (and (eql new-foreground current-foreground)
             (eql new-background current-background)
             (eql new-underline current-underline))
        current
        (make-style new-foreground new-background new-underline))))

;;;

(defparameter *default-theme*
  '((cst                        . ())

    (skipped                    . (:foreground :gray))
    (line-comment               . (:foreground :gray))
    (block-comment              . (:foreground :gray))

    (symbol                     . (:foreground :green))
    (uninterned-symbol          . (:foreground :default))
    (keyword-symbol             . (:foreground :magenta))
    (interned-symbol            . (:foreground :default))
    (lambda-list-keyword-symbol . (:foreground :magenta))
    (two-package-markers        . (:background :red))

    (number                     . (:foreground :blue))

    (cons                       . ())
    (vector                     . (:foreground :yellow))
    (array                      . (:foreground :yellow))
    (character                  . (:foreground :yellow))
    (string                     . (:foreground :yellow))
    (pathname                   . (:foreground :yellow :underlinep t))

    (structure                  . ())

    (feature-expression         . (:background :yellow))

    (function                   . (:background :blue))
    (quote                      . (:background :gray))
    (quasiquote                 . (:background :black))
    (unquote                    . (:background :default))

    ;; Matching
    ((open  1)                  . (:foreground :blue))
    ((open  2)                  . (:foreground :magenta))
    ((open  3)                  . (:foreground :green))
    ((open  4)                  . (:foreground :yellow))
    ((open  5)                  . (:foreground :cyan))
    ((close 1)                  . (:foreground :blue))
    ((close 2)                  . (:foreground :magenta))
    ((close 3)                  . (:foreground :green))
    ((close 4)                  . (:foreground :yellow))
    ((close 5)                  . (:foreground :cyan))

    ;; Error
    (error                      . (:foreground :red :underlinep t))))

(defmethod style-for-class ((class t) (theme t))
  (multiple-value-bind (style foundp)
      (a:assoc-value theme class :test #'equal)
    (if foundp
        style
        (error "No style for ~A" class))))

;;; Client

(defclass ansi-text-client (stream-mixin
                            nesting-tracking-mixin)
  ((%theme       :initarg  :theme
                 :reader   theme
                 :initform *default-theme*)
   ;; State
   (%style-stack :accessor style-stack
                 :initform (list (make-style :default :default nil))))
  (:default-initargs
   :stream (a:required-argument :stream)))

(defmethod style ((client ansi-text-client))
  (first (style-stack client)))

(defmethod push-style! (style (client ansi-text-client))
  (push style (style-stack client))
  (apply-style client))

(defmethod pop-style! ((client ansi-text-client))
  (pop (style-stack client))
  (apply-style client))

(defmethod apply-style ((client ansi-text-client))
  (let ((style (style client)))
    (format (stream client) "~C[~D;~D;~:[24~;4~]m"
            #\Escape
            (ecase (background style)
              (:black    40)
              (:red      41)
              (:green    42)
              (:yellow   43)
              (:blue     44)
              (:magenta  45)
              (:cyan     46)
              (:white    47)
              (:default  49)
              (:gray    100))
            (ecase (foreground style)
              (:black         30)
              (:red           31)
              (:green         32)
              (:yellow        33)
              (:blue          34)
              (:magenta       35)
              (:cyan          36)
              (:white         37)
              (:default       39)
              (:gray          90)
              (:bright-red    91)
              (:bright-green  92)
              (:bright-yellow 93)
              (:bright-white  97))
            (underlinep style))))

;;; Defaults

(defmethod style-class ((client ansi-text-client) (node t))
  (let* ((name    (symbol-name (class-name (class-of node))))
         (trimmed (subseq name 0 (- (length name) (length "-node")))))
    (find-symbol trimmed (load-time-value '#:eclector.examples.highlight))))

(defmethod enter-node ((client ansi-text-client) (node t))
  (let* ((style-class (a:ensure-car (style-class client node)))
         (style       (style-for-class style-class (theme client)))
         (new-style   (apply #'change-style (style client) style)))
    (push-style! new-style client)))

(defmethod leave-node ((client ansi-text-client) (node t))
  (pop-style! client))

(defmethod write-character ((client    ansi-text-client)
                            (position  t)
                            (character t)
                            (node      t))
  (write-char character (stream client)))

;;; Document

(defmethod enter-node ((client ansi-text-client) (node cst::cst))
  (apply-style client))

(defmethod leave-node ((client ansi-text-client) (node cst::cst))
  (finish-output (stream client)))

;;; Symbol

#+later (defmethod leave-node :before ((client t) (node interned-symbol-node) (stream t))
  (when (intern? node)
    (format stream "<span class=\"message\">~A</span>" "Do not use unexported symbols.")))

#+later (defmethod style-class ((client t) (node interned-symbol-node)) ; TODO return list and use APPEND method combination?
  (if (intern? node)
      (list* "two-package-markers" (a:ensure-list (call-next-method)))
      (call-next-method)))

#+later (defun name-of-package? (string package)
  (or (string= string (package-name package))
      (member string (package-nicknames package) :test #'string=)))

;;; Errors

(defmethod enter-errors ((client ansi-text-client) (errors sequence))
  (let* ((style     (style-for-class 'error (theme client)))
         (new-style (apply #'change-style (style client) style)))
    (push-style! new-style client)))

(defmethod leave-errors ((client ansi-text-client) (errors sequence))
  (pop-style! client))

#+no (defmethod enter-errors ((client ansi-text-client) (errors sequence))
       (format (stream client) "~C[4;31m" #\Escape))

#+no (defmethod leave-errors ((client ansi-text-client) (errors sequence))
  (let ((stream (stream client)))
    (write-string "[" stream)
    (loop :for (error rest) :on errors
          :do (write-string (message error) stream)
          :when rest
          :do (write-string "/" stream))
    (write-string "]" stream)
    (format stream "~C[4;39m" #\Escape)))

;;; `nesting-highlighting-mixin'

(defclass nesting-highlighting-mixin ()
  ((%point :initarg :point
           :reader  point)))

(defmethod write-character :around ((client    nesting-highlighting-mixin)
                                    (position  t)
                                    (character t)
                                    (node      cst::sequence-node))
  (let* ((children (cst:children node))
         (start    (cst:start node))
         (end      (cst:end node)))
    (if (<= start (point client) end)
        (let* ((child-start  (a:when-let ((child (first children)))
                               (cst:start child)))
               (child-end    (a:when-let ((child (a:lastcar children)))
                               (cst:end child)))

               (open-start?  (and (eql position start) (not (eql start child-start))))
               (open-end?    (or (and (not child-start) (eql position start))
                                 (eql (1+ position) child-start)))

               (close-start? (or (and (not child-end) (eql position (1- end)))
                                 (eql position child-end)))
               (close-end?   (and (eql position (1- end)) (not (eql end child-end)))))
          (when open-start?
            (let* ((name      `(open ,(nesting-depth client)))
                   (style     (style-for-class name (theme client)))
                   (new-style (apply #'change-style (style client) style)))
              (push-style! new-style client)))
          (when close-start?
            (let* ((name      `(close ,(nesting-depth client)))
                   (style     (style-for-class name (theme client)))
                   (new-style (apply #'change-style (style client) style)))
              (push-style! new-style client)))
          (call-next-method)
          (when open-end?
            (pop-style! client))
          (when close-end?
            (pop-style! client)))
        (call-next-method))))
