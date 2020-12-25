(cl:in-package #:eclector.examples.highlight)

(defun render (client input-string cst errors)
  (let ((node cst)
        ; (stack (list cst))
        )
    (flet ((maybe-end-errors (position)
             (a:when-let ((errors (remove position errors
                                          :test-not #'eql :key #'end)))
               (leave-errors client errors)))
           (maybe-start-errors (position)
             (a:when-let ((errors (remove position errors
                                          :test-not #'eql :key #'start)))
               (enter-errors client errors)))
           (maybe-leave-nodes (position)
             (loop :while (eql position (end node))
                   :do (leave-node client node)
                       ; (pop stack)
                       (setf node (parent node))))
           (maybe-enter-node (position)
             (a:when-let ((child (find position (children node) :key #'start)))
               (enter-node client child)
               ; (push child stack)
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
                       (when (and (eql character #\Newline)
                                  (find end errors :test #'eql :key #'end))
                         (write-char #\¶ (stream client)))
                       (maybe-end-errors end)

                       (loop :while node
                             :do (leave-node client node)
                                 (setf node (parent node)))
                       #+no (map nil (lambda (node)
                                  (leave-node client node))
                            stack))))))
