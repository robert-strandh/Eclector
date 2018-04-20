(cl:defpackage #:eclector.reader
  (:use #:common-lisp)
  ;; When the reader is compiled for the purpose of cross compilation,
  ;; we must shadow a certain number of symbols that would otherwise
  ;; clash with the corresponding symbols in the host package
  ;; COMMON-LISP.
  (:shadow
   #:read
   #:read-preserving-whitespace)
  ;; This one we just shadow because we define our own.
  (:shadow
   #:reader-error)
  ;; Contrary to other variables affecting the reader, we cannot use
  ;; the host version of *READTABLE* because we do not necessarily
  ;; use the same representation of readtables as the host does, and
  ;; Common Lisp does not have a standardized API for manipulating
  ;; readtables.  Perhaps we should write a CDR (Common Lisp Document
  ;; Repository) document suggesting such an API.
  (:shadowing-import-from #:eclector.readtable
   #:*readtable*)
  (:export
   #:readtable
   #:*readtable*
   #:*client*
   #:*skip-reason*
   #:read
   #:read-preserving-whitespace
   #:*preserve-whitespace*
   #:read-common
   #:read-token
   #:note-skipped-input
   #:interpret-token
   #:interpret-symbol
   #:call-reader-macro
   #:fixup
   ;; Backquote customization.
   #:wrap-in-quasiquote
   #:wrap-in-unquote
   #:wrap-in-unquote-splicing
   ;; Names of additional conditions.
   #:backquote-condition
   #:invalid-context-for-backquote
   #:comma-not-inside-backquote
   #:unquote-splicing-in-dotted-list
   #:undefined-use-of-backquote
   #:invalid-context-for-consing-dot
   #:consing-dot-most-be-followed-by-object
   #:multiple-objects-following-consing-dot
   #:invalid-context-for-right-parenthesis
   #:sub-char-must-not-be-a-decimal-digit
   #:char-must-be-a-dispatching-character
   #:symbol-name-must-not-end-with-package-marker
   #:symbol-does-not-exist
   #:symbol-is-not-external
   #:two-package-markers-must-be-adjacent
   #:two-package-markers-must-not-be-first
   #:symbol-can-have-at-most-two-package-markers
   #:numeric-parameter-supplied-but-ignored
   #:numeric-parameter-not-supplied-but-required
   #:unknown-character-name
   #:digit-expected
   #:invalid-radix
   #:invalid-default-float-format
   #:too-many-elements
   #:no-elements-found
   #:single-feature-expected
   #:sharpsign-equals-label-defined-more-than-once
   #:sharpsign-sharpsign-undefined-label
   ;; Names of macros related to backquote.
   ;; We export them so that the pretty printer
   ;; can use them properly.
   #:quasiquote #:unquote #:unquote-splicing
   ))
