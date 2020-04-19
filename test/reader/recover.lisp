(cl:in-package #:eclector.reader.test)

(in-suite :eclector.reader)

(test recover/smoke
  "Test recovering from various syntax errors."

  (mapc (alexandria:named-lambda one-case (input-and-expected)
          (destructuring-bind (input expected-conditions expected-value
                               &optional (expected-position (length input)))
              input-and-expected
            (let ((remaining-conditions expected-conditions))
              (flet ((do-it ()
                       (handler-bind
                           ((error
                              (lambda (condition)
                                (let ((expected-condition (pop remaining-conditions)))
                                  (is (typep condition expected-condition)
                                      "For input ~S, expected a ~
                                       condition of type ~S but got ~
                                       ~S."
                                      input expected-condition condition))
                                (let ((restart (find-restart 'eclector.reader:recover)))
                                  (is-true (typep restart 'restart)
                                           "For input ~S expected a RECOVER restart."
                                           input)
                                  (unless restart
                                    (return-from one-case))
                                  (is (not (string= "" (princ-to-string restart)))
                                      "For input ~S expected restart to print properly."
                                      input)
                                  (invoke-restart restart)))))
                         (with-input-from-string (stream input)
                           (values (let ((eclector.reader::*backquote-depth* 1)
                                         (eclector.reader::*client*
                                           (make-instance 'sharpsign-s-client)))
                                     (eclector.reader:read stream nil))
                                   (file-position stream))))))
                ;; Check expected value and position.
                (multiple-value-bind (value position) (do-it)
                  (is (relaxed-equalp expected-value value)
                      "For input ~S, expected return value ~S but got ~
                       ~S."  input expected-value value)
                  (is (equalp expected-position position)
                      "For input ~S, expected position ~S but got ~S."
                      input expected-position position))
                ;; All signaled conditions were as expected. Make sure
                ;; all expected conditions were signaled.
                (is (null remaining-conditions)
                    "For input ~S, expected condition~P ~S but those ~
                     were not signaled."
                    input
                    (length remaining-conditions) remaining-conditions)))))

        `(;; Recover from invalid syntax in symbols.
          (,(format nil ":foo~C" #\Backspace) (eclector.reader:invalid-constituent-character)                :foo_)
          (":fo\\"                            (eclector.reader:unterminated-single-escape-in-symbol)         :fo)
          (":fo|o"                            (eclector.reader:unterminated-multiple-escape-in-symbol)       :fo|o|)
          ("foo:"                             (eclector.reader:symbol-name-must-not-end-with-package-marker) foo|:|)
          (":foo:bar"                         (eclector.reader:two-package-markers-must-be-adjacent)         :foo|:|bar)
          ("::foo"                            (eclector.reader:two-package-markers-must-not-be-first)        :foo)
          ("eclector.reader.test:::foo"       (eclector.reader:symbol-can-have-at-most-two-package-markers)  |:|foo)

          ;; Recover from invalid number tokens.
          ("3/0" (eclector.reader:zero-denominator) 3)

          ;; Single quote
          ("'"         (eclector.reader:end-of-input-after-quote) 'nil)
          ("(')"       (eclector.reader:object-must-follow-quote) ('nil))

          ;; Double quote
          ("\""    (eclector.reader:unterminated-string)                  "")
          ("\"ab"  (eclector.reader:unterminated-string)                  "ab")
          ("\"a\\" (eclector.reader:unterminated-single-escape-in-string
                    eclector.reader:unterminated-string)                  "a")

          ;; Recover from quasiquotation-related errors.
          ("`"         (eclector.reader:end-of-input-after-backquote)           (eclector.reader:quasiquote nil))
          ("(`)"       (eclector.reader:object-must-follow-backquote)           ((eclector.reader:quasiquote nil)))

          ("`(1 ,)"    (eclector.reader:object-must-follow-unquote)             (eclector.reader:quasiquote
                                                                                 (1 (eclector.reader:unquote nil))))
          ("`,"        (eclector.reader:end-of-input-after-unquote)             (eclector.reader:quasiquote
                                                                                 (eclector.reader:unquote nil)))

          ("#C(,1 2)"  (eclector.reader:unquote-in-invalid-context)             #C(1 2))
          ("#C(`,1 2)" (eclector.reader:backquote-in-invalid-context
                        eclector.reader:unquote-not-inside-backquote)
                                                                                #C(1 2))

          ;; Recover from list-related errors
          ("("         (eclector.reader:unterminated-list)                      ())
          ("(1 2"      (eclector.reader:unterminated-list)                      (1 2))
          ("(1 ."      (eclector.reader:end-of-input-after-consing-dot
                        eclector.reader:unterminated-list)
                                                                                (1))
          ("(1 .)"     (eclector.reader:object-must-follow-consing-dot)         (1))
          ("(1 . 2 3)" (eclector.reader:multiple-objects-following-consing-dot) (1 . 2))
          (")(1)"      (eclector.reader:invalid-context-for-right-parenthesis)  (1))

          ;; Recover from errors related to read-time evaluation.
          ("#."             (eclector.reader:end-of-input-after-sharpsign-dot) nil)
          ("(#.)"           (eclector.reader:object-must-follow-sharpsign-dot) (nil))
          ("#.(error \"\")" (eclector.reader:read-time-evaluation-error)       nil)

          ;; Recover from vector-related errors
          ("#("        (eclector.reader:unterminated-vector) #())
          ("#(1 2"     (eclector.reader:unterminated-vector) #(1 2))
          ("#1()"      (eclector.reader:no-elements-found)   #())
          ("#1(1 2)"   (eclector.reader:too-many-elements)   #(1))

          ;; Recover from errors in SHARPSIGN-BACKSLASH
          ("#\\"         (eclector.reader:end-of-input-after-backslash)                   #\?)
          ("#\\a\\"      (eclector.reader:unterminated-single-escape-in-character-name)   #\a)
          ("#\\a|"       (eclector.reader:unterminated-multiple-escape-in-character-name) #\a)
          ("#\\Return\\" (eclector.reader:unterminated-single-escape-in-character-name)   #\Return)
          ("#\\Return|"  (eclector.reader:unterminated-multiple-escape-in-character-name) #\Return)
          ("#\\Nosuch"   (eclector.reader:unknown-character-name)                         #\?)
          ("#\\Nosuch\\" (eclector.reader:unterminated-single-escape-in-character-name
                          eclector.reader:unknown-character-name)
                                                                                          #\?)

          ;; Recover from errors in READ-RATIONAL.
          ("#b"      (eclector.reader:end-of-input-before-digit) 1)
          ("#b)"     (eclector.reader:digit-expected)            #b0     2)
          ("#b|"     (eclector.reader:digit-expected)            #b0     2)
          ("#b121"   (eclector.reader:digit-expected)            #b111)
          ("#b1/"    (eclector.reader:end-of-input-before-digit) #b1/1)
          ("#b1/)"   (eclector.reader:digit-expected)            #b1     4)
          ("#b1/|"   (eclector.reader:digit-expected)            #b1     4)
          ("#b1/1|"  (eclector.reader:digit-expected)            #b1     5)
          ("#b1/121" (eclector.reader:digit-expected)            #b1/111 7)
          ("#b1/0"   (eclector.reader:zero-denominator)          #b1/1)

          ;; Recover from errors related to bit-vector literals
          ("#1*"       (eclector.reader:no-elements-found) #*)
          ("#1*11"     (eclector.reader:too-many-elements) #*1)
          ("#*021"     (eclector.reader:digit-expected)    #*001)

          ;; Recover from block-comment-related errors
          ("#|"        (eclector.reader:unterminated-block-comment)             nil)
          ("#|foo"     (eclector.reader:unterminated-block-comment)             nil)

          ;; Recover from errors related to SHARPSIGN-SINGLE-QUOTE
          ("#'"   (eclector.reader:end-of-input-after-sharpsign-single-quote) nil)
          ("(#')" (eclector.reader:object-must-follow-sharpsign-single-quote) (nil))

          ;; Recover from general array-related errors
          ("#2A"            (eclector.reader:end-of-input-after-sharpsign-a)  #2A())
          ("(#2A)"          (eclector.reader:object-must-follow-sharpsign-a)  (#2A()))
          ("#2A("           (eclector.reader:unterminated-list)               #2A())
          ("#2A(1)"         (eclector.reader:read-object-type-error)          #2A())
          ("#2A((1) (1 2))" (eclector.reader:incorrect-initialization-length) #2A())

          ;; Recover from errors related to uninterned symbols
          ("#::foo"    (eclector.reader:uninterned-symbol-must-not-contain-package-marker) #:|:|foo)
          ("#:foo:"    (eclector.reader:uninterned-symbol-must-not-contain-package-marker) #:foo|:|)
          ("#:fo\\"    (eclector.reader:unterminated-single-escape-in-symbol)              #:fo)
          ("#:fo|o"    (eclector.reader:unterminated-multiple-escape-in-symbol)            #:fo|o|)

          ;; Recover from complex-related errors
          ("#C"          (eclector.reader:end-of-input-after-sharpsign-c)        #C(1 1))
          ("#C1"         (eclector.reader:non-list-following-sharpsign-c)        #C(1 1))
          ("#C||"        (eclector.reader:non-list-following-sharpsign-c)        #C(1 1))
          ("#C)"         (eclector.reader:complex-parts-must-follow-sharpsign-c) #C(1 1) 2)
          ("#C("         (eclector.reader:end-of-input-before-complex-part)      #C(1 1))
          ("#C()"        (eclector.reader:complex-part-expected)                 #C(1 1))
          ("#C(2"        (eclector.reader:end-of-input-before-complex-part)      #C(2 1))
          ("#C(2)"       (eclector.reader:complex-part-expected)                 #C(2 1))
          ("#C(2 3"      (eclector.reader:unterminated-list)                     #C(2 3))
          ("#C(2 3 4)"   (eclector.reader:too-many-complex-parts)                #C(2 3))
          ("#C(2 3 4 5)" (eclector.reader:too-many-complex-parts)                #C(2 3))
          ("#C(#\\a 2)"  (eclector.reader:read-object-type-error)                #C(1 2))

          ;; Recover from structure-literal-related errors
          ("#S"            (eclector.reader:end-of-input-after-sharpsign-s)                nil)
          ("#S1"           (eclector.reader:non-list-following-sharpsign-s)                nil)
          ("#S1"           (eclector.reader:non-list-following-sharpsign-s)                nil)
          ("#S)"           (eclector.reader:structure-constructor-must-follow-sharpsign-s) nil 2)
          ("#S("           (eclector.reader:end-of-input-before-structure-type-name)       nil)
          ("#S()"          (eclector.reader:no-structure-type-name-found)                  nil)
          ("#S(1)"         (eclector.reader:structure-type-name-is-not-a-symbol)           nil)
          ("#S(foo"        (eclector.reader:end-of-input-before-slot-name)                 (foo))
          ("#S(foo 1)"     (eclector.reader:slot-name-is-not-a-string-designator
                            eclector.reader:no-slot-value-found)
                                                                                           (foo))
          ("#S(foo :bar"   (eclector.reader:end-of-input-before-slot-value)                (foo))
          ("#S(foo :bar)"  (eclector.reader:no-slot-value-found)                           (foo))
          ("#S(foo :bar 1" (eclector.reader:end-of-input-before-slot-name)                 (foo :bar 1))

          ("#"         (eclector.reader:unterminated-dispatch-macro)            nil)

          ;; Multiple subsequent recoveries needed.
          ("(1 (2"     (eclector.reader:unterminated-list
                        eclector.reader:unterminated-list)
                                                                                (1 (2)))
          ("(1 \"a"    (eclector.reader:unterminated-string
                        eclector.reader:unterminated-list)
                                                                                (1 "a")))))
