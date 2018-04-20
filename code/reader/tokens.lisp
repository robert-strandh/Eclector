(cl:in-package #:eclector.reader)

(defmethod interpret-symbol (token
                             position-package-marker-1
                             position-package-marker-2
                             input-stream)
  (cond ((null position-package-marker-1)
         (intern token *package*))
        ((null position-package-marker-2)
         (cond ((= position-package-marker-1 (1- (length token)))
                (%reader-error input-stream
                               'symbol-name-must-not-end-with-package-marker
                               :desired-symbol token))
               ((= position-package-marker-1 0)
                (intern (subseq token 1) '#:keyword))
               (t
                (let ((symbol-name (subseq token (1+ position-package-marker-1)))
                      (package-name (subseq token 0 position-package-marker-1)))
                  (multiple-value-bind (symbol status)
                      ;; If the package doesn't exist find-symbol will signal an error
                      ;; (hopefully? Doesn't seem to be defined, but it's usual)
                      ;; so the later find-packages should be okay.
                      (find-symbol symbol-name package-name)
                    (cond ((null status)
                           (%reader-error input-stream 'symbol-does-not-exist
                                          :symbol-name symbol-name
                                          :package (find-package package-name)))
                          ((eq status :internal)
                           (%reader-error input-stream 'symbol-is-not-external
                                          :symbol-name symbol-name
                                          :package (find-package package-name)))
                          (t symbol)))))))
        (t
         (if (= position-package-marker-2 (1- (length token)))
             (%reader-error input-stream 'symbol-name-must-not-end-with-package-marker
                            :desired-symbol token)
             (intern (subseq token (1+ position-package-marker-2))
                     (subseq token 0 position-package-marker-1))))))

(declaim (inline exponent-marker-p))
(defun exponent-marker-p (char)
  (member char '(#\e #\E #\f #\F #\s #\S #\d #\D #\l #\L) :test #'char=))

(declaim (inline reader-float-format))
(defun reader-float-format (&optional (exponent-marker #\E))
  (ecase exponent-marker
    ((#\e #\E)
     (case *read-default-float-format*
       (single-float 'single-float)
       (short-float 'short-float)
       (double-float 'double-float)
       (long-float 'long-float)
       (t
        ;; *read-default-float-format* may be some other type
        ;; *specifier which the implementation chooses to allow
        (if (subtypep *read-default-float-format* 'float)
            *read-default-float-format*
            (error 'invalid-default-float-format ; FIXME this is currently a READER-ERROR, but we do not have a stream at this point
                   :float-format *read-default-float-format*)))))
    ((#\f #\F) 'single-float)
    ((#\s #\S) 'short-float)
    ((#\d #\D) 'double-float)
    ((#\l #\L) 'long-float)))

(declaim (ftype (function (&key (:base (integer 2 36))) function)
                make-integer-accumulator))
(defun make-integer-accumulator (&key (base 10.))
  (let ((value 0))
    (lambda (&optional char)
      (if char
          (let ((digit (digit-char-p char base)))
            (when digit
              (setf value (+ (* value base) digit))
              t))
          value))))

(defmethod interpret-token (token token-escapes input-stream)
  (convert-according-to-readtable-case token token-escapes)
  (let ((length (length token))
        (sign 1)
        (decimal-mantissa (make-integer-accumulator))
        (mantissa/numerator (make-integer-accumulator :base *read-base*))
        (denominator (make-integer-accumulator :base *read-base*))
        (fraction-numerator (make-integer-accumulator))
        (fraction-denominator 1)
        (exponent-sign 1)
        (exponent (make-integer-accumulator))
        (exponent-marker nil)
        (position-package-marker-1 nil)
        (position-package-marker-2 nil)
        (index -1))
    ;; The NEXT function and the NEXT-COND macro handle fetching the
    ;; next character and returning a symbol and going to tag SYMBOL
    ;; in case of as escape and as the default successor state.
    (flet ((next ()
             (incf index)
             (if (= length index)
                 nil
                 (values (aref token index) (aref token-escapes index)))))
      (macrolet ((next-cond ((char-var &optional
                                       return-symbol-if-eoi
                                       (colon-go-symbol t))
                             &body clauses)
                   (alexandria:with-unique-names (escapep-var)
                     `(multiple-value-bind (char ,escapep-var) (next)
                        (cond
                          ,@(when return-symbol-if-eoi
                              `(((null ,char-var)
                                 (return-from interpret-token
                                   (interpret-symbol token
                                                     position-package-marker-1
                                                     position-package-marker-2
                                                     input-stream)))))
                          (,escapep-var (go symbol))
                          ,@(when colon-go-symbol
                              `(((eql char #\:)
                                 (setf position-package-marker-1 index)
                                 (go symbol))))
                          ,@clauses
                          (t (go symbol)))))))
        (tagbody
         start
           ;; If we have a token of length 0, it must be a symbol in
           ;; the current package.
           (next-cond (char t)
             ((eql char #\+)
              (go sign))
             ((eql char #\-)
              (setf sign -1)
              (go sign))
             ((funcall decimal-mantissa char)
              (funcall  mantissa/numerator char)
              (go decimal-integer))
             ((funcall mantissa/numerator char)
              (go integer))
             ((eql char #\.)
              (go dot)))
         sign             ; We have a sign, i.e., #\+ or #\-.
           ;; If a sign is all we have, it is a symbol.
           (next-cond (char t)
             ((funcall decimal-mantissa char)
              (funcall mantissa/numerator char)
              (go decimal-integer))
             ((funcall mantissa/numerator char)
              (go integer))
             ((eql char #\.)
              (go sign-dot)))
         dot
           (next-cond (char)
             ((not char)
                 (if *consing-dot-allowed-p*
                     (return-from interpret-token
                       *consing-dot*)
                     (%reader-error input-stream 'invalid-context-for-consing-dot)))
             ((funcall fraction-numerator char)
              (setf fraction-denominator
                    (* fraction-denominator 10))
              (go float-no-exponent)))
         sign-dot                       ; sign decimal-point
           ;; If all we have is a sign followed by a dot, it must be a
           ;; symbol in the current package.
           (next-cond (char t)
             ((funcall fraction-numerator char)
              (setf fraction-denominator
                    (* fraction-denominator 10))
              (go float-no-exponent)))
         decimal-integer                ; [sign] decimal-digit+
           (next-cond (char)
             ((not char)
              (return-from interpret-token
                (* sign (funcall mantissa/numerator))))
             ((eql char #\.)
              (go decimal-integer-final))
             ((funcall decimal-mantissa char)
              (funcall mantissa/numerator char)
              (go decimal-integer))
             ((funcall mantissa/numerator char)
              (go integer))
             ((eql char #\/)
              (go ratio-start))
             ((exponent-marker-p char)
              (setf exponent-marker char)
              (go float-exponent-start)))
         decimal-integer-final   ; [sign] decimal-digit+ decimal-point
           (next-cond (char)
             ((not char)
              (return-from interpret-token
                (* sign (funcall decimal-mantissa))))
             ((funcall fraction-numerator char)
              (setf fraction-denominator
                    (* fraction-denominator 10))
              (go float-no-exponent))
             ((exponent-marker-p char)
              (setf exponent-marker char)
              (go float-exponent-start)))
         integer                 ; [sign] digit+
           ;; At least one digit is not decimal.
           (next-cond (char)
             ((not char)
              (return-from interpret-token
                (* sign (funcall mantissa/numerator))))
             ((funcall mantissa/numerator char)
              (go integer))
             ((eql char #\/)
              (go ratio-start)))
         ratio-start                    ; [sign] digit+ /
           (next-cond (char t)
             ((funcall denominator char)
              (go ratio)))
         ratio                          ; [sign] digit+ / digit+
           (next-cond (char)
             ((not char)
              (return-from interpret-token
                (* sign (/ (funcall mantissa/numerator)
                           (funcall denominator)))))
             ((funcall denominator char)
              (go ratio)))
         float-no-exponent
           ;; [sign] decimal-digit* decimal-point decimal-digit+
           (next-cond (char)
             ((not char)
              (return-from interpret-token
                (coerce (* sign
                           (+ (funcall mantissa/numerator)
                              (/ (funcall fraction-numerator)
                                 fraction-denominator)))
                        (reader-float-format))))
             ((funcall fraction-numerator char)
              (setf fraction-denominator
                    (* fraction-denominator 10))
              (go float-no-exponent))
             ((exponent-marker-p char)
              (setf exponent-marker char)
              (go float-exponent-start)))
         float-exponent-start
           ;; [sign] decimal-digit+ exponent-marker
           ;; or
           ;; [sign] decimal-digit* decimal-point decimal-digit+ exponent-marker
           (next-cond (char t)
             ((eq char #\+)
              (go float-exponent-sign))
             ((eq char #\-)
              (setf exponent-sign -1)
              (go float-exponent-sign))
             ((funcall exponent char)
              (go float-exponent)))
         float-exponent-sign
           ;; [sign] decimal-digit+ exponent-marker sign
           ;; or
           ;; [sign] decimal-digit* decimal-point decimal-digit+ exponent-marker sign
           (next-cond (char t)
             ((funcall exponent char)
              (go float-exponent)))
         float-exponent
           ;; [sign] decimal-digit+ exponent-marker [sign] digit+
           ;; or
           ;; [sign] decimal-digit* decimal-point decimal-digit+
           ;; exponent-marker [sign] digit+
           (next-cond (char)
             ((not char)
              (return-from interpret-token
                (coerce (* sign
                           (+ (funcall mantissa/numerator)
                              (/ (funcall fraction-numerator)
                                 fraction-denominator))
                           (expt 10 (* exponent-sign (funcall exponent))))
                        (reader-float-format exponent-marker))))
             ((funcall exponent char)
              (go float-exponent)))
         symbol
           ;; a sequence of symbols denoting a valid symbol name, except
           ;; that the last character might be a package marker.
           (next-cond (char t nil)
             ((eq char #\:)
              (cond ((null position-package-marker-1)
                     (setf position-package-marker-1 index))
                    ((null position-package-marker-2)
                     (cond ((/= position-package-marker-1 (1- index))
                            (%reader-error
                             input-stream 'two-package-markers-must-be-adjacent
                             :desired-symbol token))
                           ((= position-package-marker-1 0)
                            (%reader-error
                             input-stream 'two-package-markers-must-not-be-first
                             :desired-symbol token))
                           (t
                            (setf position-package-marker-2 index))))
                    (t
                     (%reader-error input-stream 'symbol-can-have-at-most-two-package-markers
                                    :desired-symbol token)))
              (go symbol))))))))
