(cl:in-package #:eclector.examples.highlight)

(defun highlight (input-string &key (package "COMMON-LISP-USER")
                                    (client (make-instance 'minimal-html-client :stream *standard-output*)))
  (multiple-value-bind (cst errors) (read-stuff input-string :package package)
    (render client input-string cst errors)))

(defun highlight-string (string &key package client)
  (with-output-to-string (stream)
    (reinitialize-instance client :stream stream)
    (apply #'highlight string :client client (when package
                                               (list :package package)))))

(defun process (input output &key (client (make-instance 'linking-html-client :input input :stream *standard-output*)))
  (a:with-output-to-file (stream output :if-exists :supersede)
    (reinitialize-instance client :stream stream)
    (apply #'highlight input (when client
                               (list :client client)))))
