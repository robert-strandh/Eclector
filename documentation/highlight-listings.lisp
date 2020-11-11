(require :asdf)
(map nil #'asdf:load-asd
     '(#P"../examples/highlight/eclector.examples.highlight.asd"
       #P "../eclector-concrete-syntax-tree.asd"))
(map nil #'asdf:load-system
     '("eclector.examples.highlight" "eclector-concrete-syntax-tree"))

;;;

(defun replace-in-string (replacement pattern string)
  (with-output-to-string (stream)
    (loop :for previous-end = 0 :then end
          :for (start end processp) = (multiple-value-list
                                       (funcall pattern string previous-end))
          :while start
          ; :do (format t "~D:~D -> ~A~%" start end (subseq string start end))
          :do (write-string string stream :start previous-end :end start)
              (if processp
                  (write-string (funcall replacement string start end) stream)
                  (write-string string stream :start start :end end))
          :finally (write-string string stream :start previous-end))))

(defun replace-all-in-string (replacements string)
  (replace-in-string
   (lambda (string start end)
     (loop :for (pattern replacement) :in replacements
           :when (string= pattern string :start2 start :end2 end)
           :do (return replacement)))
   (lambda (string start)
     (values-list
      (reduce (lambda (best-position pattern)
                (let ((position (search pattern string :start2 start)))
                  (if (and position
                           (or (null best-position)
                               (< position (first best-position))))
                      (list position (+ position (length pattern)) t)
                      best-position)))
              replacements :key #'first :initial-value nil)))
   string))

;;;

(defun replace-entities (string)
  (replace-all-in-string '(("&quot;" "\"")
                           ("&amp;"  "&")
                           ("&lt;"   "<")
                           ("&gt;"   ">")
                           ("&rArr;" "→"))
                         string))

;;; Eclector links

(defun escape-for-anchor (string)
  (replace-all-in-string
   '(("*" "_002a") ("-" "_002d") ("." "_002e") ("[" "_005b") ("]" "_005d"))
   string))

(defun name->anchor (symbol-name &optional package-name)
  (format nil "~A~@[-~A~]"
          (escape-for-anchor (string-downcase symbol-name))
          (when package-name
            (escape-for-anchor (format nil "[~(~A~)]" package-name)))))

(defclass documentation-client (eclector.examples.highlight::link-mixin
                                eclector.examples.highlight::html-fragment-mixin
                                eclector.examples.highlight::html-client)
  ())

#+no (defmethod eclector.examples.highlight::url :around ((client documentation-client)
                                                     (node   eclector.examples.highlight::interned-symbol-node))
  (let ((url (call-next-method)))
    (when url
      (format t "~A -> ~A~%" node url))
    url))

(defmethod eclector.examples.highlight::url ((client documentation-client)
                                             (node   eclector.examples.highlight::interned-symbol-node))
  (let ((name    (eclector.examples.highlight::name node))
        (package (eclector.examples.highlight::package node)))
    (if (and (or (string= package "ECLECTOR.READER")
                 (string= package "READTABLE") (string= package "ECLECTOR.READTABLE")
                 (string= package "PARSE-RESULT") (string= package "ECLECTOR.PARSE-RESULT")
                 (string= package "ECLECTOR.CONCRETE-SYNTAX-TREE"))
             (find-package package)
             (nth-value 1 (find-symbol name (find-package package))))
        (format nil "#index-~A" (name->anchor name package))
        (call-next-method))))

(defmethod eclector.examples.highlight::write-character ((client    documentation-client)
                                                         (position  t)
                                                         (character t)
                                                         (node      eclector.examples.highlight::interned-symbol-node))
  (let ((name (eclector.examples.highlight::name node)))
    (if (or (eql 0 (search "<EM>" name))
            (eql 0 (search "<VAR>" name)))
        (let ((relative-position (- position (eclector.examples.highlight::start node))))
          (if (or (<= relative-position 4)
                  (>= relative-position (- (length name) 6)))
              (write-char (char-downcase character) (eclector.examples.highlight::stream client))
              (call-next-method)))
        (call-next-method))))

;;;

(defun find-package-annotation (string start)
  (let* ((start-string "are in the <tt>")
         (start-index  (search start-string string :start2 start))
         (end-index    (when start-index
                         (search "</tt> package" string :start2 start-index))))
    #+no (when start-index
      (format t "~D:~D ~A ~A~%" start-index end-index (subseq string start-index end-index) (> (- end-index start-index) 2)))
    (when start-index
      (values start-index end-index t))))

(defun find-inline-tt (string start)
  (let* ((start-string "<tt>")
         (start-index  (search start-string string :start2 start))
         (end-index    (when start-index
                         (search "</tt>" string :start2 start-index)))
         (start-index  (when end-index
                         (+ start-index (length start-string)))))
    #+no (when start-index
      (format t "~D:~D ~A ~A~%" start-index end-index (subseq string start-index end-index) (> (- end-index start-index) 2)))
    (when start-index
      (values start-index end-index (not (and (<= (- end-index start-index) 2)
                                              (not (alphanumericp (aref string start-index)))))))))

(defun find-inline-code (string start)
  (let* ((start-string "<code>")
         (start-index  (search start-string string :start2 start))
         (end-index    (when start-index
                         (search "</code>" string :start2 start-index)))
         (start-index  (when end-index
                         (+ start-index (length start-string)))))
    #+no (when start-index
      (format t "~D:~D ~A ~A~%" start-index end-index (subseq string start-index end-index) (> (- end-index start-index) 2)))
    (when (and start-index)
      (values start-index end-index (not (or (and (<= (- end-index start-index) 2)
                                                  (not (alphanumericp (aref string start-index))))
                                             ;; Don't try to parse index entries
                                             (char= #\: (aref string (1- end-index)))))))))

(defun find-listing (string start)
  (let* ((start-string "<pre class=\"lisp\">")
         (start-index  (search start-string string :start2 start))
         (end-index    (when start-index
                         (search "</pre>" string :start2 start-index))))
    (when start-index
      (values (+ start-index (length start-string)) end-index t))))

(defun find-code (string start)
  (values-list
   (reduce (lambda (best-position function)
             (multiple-value-bind (start end processp)
                 (funcall function string start)
               (if (and start
                        (or (null best-position)
                            (< start (first best-position))))
                   (list start end processp)
                   best-position)))
           '(find-package-annotation find-inline-tt find-inline-code find-listing)
           :initial-value nil)))

(defun highlight-code (string)
  (let ((package "COMMON-LISP-USER"))
    (replace-in-string
     (lambda (string start end)
       (cond ((string= "are in the <tt>" string :start2 start :end2 (+ start 15))
              (let ((original (subseq string start end))
                    (name     (string-upcase (subseq string (+ start 15) end))))
                (format t "Switching to package ~A~%" name)
                (setf package name)
                original))
             (t
              (let* ((html-string (subseq string start end))
                     (string      (replace-entities html-string))
                     (client      (make-instance 'documentation-client)))
                (eclector.examples.highlight::highlight-string string :package package
                                                                      :client  client)))))
     (lambda (string start)
       (find-code string start))
     string)))

;;;

(let* ((filename "eclector.html")
       (string   (alexandria:read-file-into-string filename))
       (result   (highlight-code string)))
  (alexandria:write-string-into-file result filename :if-exists :supersede))
