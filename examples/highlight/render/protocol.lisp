(cl:in-package #:eclector.examples.highlight.render)

;;; Client protocol

(defgeneric write-character (client position character node)
  (:documentation
   ""))

(defgeneric enter-node (client node)
  (:documentation
   ""))

(defgeneric leave-node (client node)
  (:documentation
   ""))

(defgeneric enter-errors (client errors)
  (:documentation
   ""))

(defgeneric leave-errors (client errors)
  (:documentation
   ""))

;;;

(defgeneric style-class (client node)
  (:documentation
   ""))

(defgeneric url (client node)
  (:documentation
   ""))

;;; Entry point

(defun render (client input-string cst errors)
  (let ((node cst))
    (flet ((maybe-end-errors (position)
             (a:when-let ((errors (remove position errors
                                          :test-not #'eql :key #'cst:end)))
               (leave-errors client errors)))
           (maybe-start-errors (position)
             (a:when-let ((errors (remove position errors
                                          :test-not #'eql :key #'cst:start)))
               (enter-errors client errors)))
           (maybe-leave-nodes (position)
             (loop :while (eql position (cst:end node))
                   :do (leave-node client node)
                       (setf node (cst:parent node))))
           (maybe-enter-node (position)
             (a:when-let ((child (find position (cst:children node) :key #'cst:start)))
               (enter-node client child)
               (setf node child))))
      (enter-node client cst)
      (loop :for character :across input-string
            :for position :from 0

            :do (maybe-end-errors position)
                (maybe-leave-nodes position)

                (maybe-enter-node position)
                (maybe-start-errors position)

            :do (write-character client position character node)

            :finally (let ((end (1+ position)))
                       ;; When the input ends with a newline and we
                       ;; must report errors for the end position, add
                       ;; a character to which we can attach the error
                       ;; indicator.
                       (when (and (eql character #\Newline)
                                  (find end errors :test #'eql :key #'cst:end))
                         (write-char #\¶ (stream client)))
                       (maybe-end-errors end)

                       (loop :while node
                             :do (leave-node client node)
                                 (setf node (cst:parent node))))))))
