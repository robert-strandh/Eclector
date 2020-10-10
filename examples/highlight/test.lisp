(cl:in-package #:eclector.examples.highlight)

(defun test ()
  (let ((input #+no (alexandria:read-file-into-string "~/code/cl/esrap/src/interface.lisp"
                                                      #+no "~/code/cl/eclector/examples/highlight/highlight.lisp"
                                                      #+no "~/code/cl/eclector/code/reader/macro-functions.lisp")
               "your-package::private-symbol
; bar baz
	 #'my-function  ; foo
'(quoted)
#b130303
#(1 \"bla\" '(foo) 3 &key &nothing-special &whole)
#2A((1 2 3) (4 5 6))
`(foo ,(normal)
      `(bar ,baz 3) 2)
#S(foo :bar 1)
#|foo|#
(make-instance 'hi :foo 1 :bar 2 :baz 3)
(foo bar
     (fez (:::when t
             1 2)
          #C(1 2))
     ; another comment
     #P\"bar\"
     #+sbcl #+sbcl (+ 1 2 3)
     #-sbcl :foo :bar
     :foo \"bfdafda\" 1011001)")
        (output "~/code/cl/eclector/examples/highlight/test.html"))
    (process input output)))

(defun test2 ()
  (let ((input #+no (alexandria:read-file-into-string "~/code/cl/esrap/src/interface.lisp"
                                                      #+no "~/code/cl/eclector/examples/highlight/highlight.lisp"
                                                      #+no "~/code/cl/eclector/code/reader/macro-functions.lisp")
               "your-package::private-symbol
; bar baz
	 #'my-function  ; foo
'(quoted)
#b130303
#(1 \"bla\" '(foo) 3 &key &nothing-special &whole)
#2A((1 2 3) (4 5 6))
`(foo ,(normal)
      `(bar ,baz 3) 2)
#S(foo :bar 1)
#|foo|#
(make-instance 'hi :foo 1 :bar 2 :baz 3)
(foo bar
     (fez (:::when t
             1 2)
          #C(1 2))
     ; another comment
     #P\"bar\"
     #+sbcl #+sbcl (+ 1 2 3)
     #-sbcl :foo :bar
     :foo \"bfdafda\" 1011001)"))
    (highlight input :client (make-instance 'ansi-text-client :stream sb-sys:*tty*))))
