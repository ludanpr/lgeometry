;;;; Common Lisp file:
;;;;                 handling-exceptions.lisp
;;;;
;;;; This exemplifies the exception handling of lgeometry

(in-package :cl-user)

;;; Handling distance-metric function undefined-metric condition
(defun other-metric (p1 p2)
  (- p1 p2))

(let ((p1 (lgeometry:make-point-2d-cartesian 0 0))
      (p2 (lgeometry:make-point-2d-cartesian 0 12)))
  (handler-case (lgeometry:distance-metric p1 p2 #'other-metric)
    (lgeometry:undefined-metric (condition)
      (format t "Error: ~S~%" (lgeometry:reason condition)))))

;;; Handling make-point-2d-array not-a-list condition
(handler-case (lgeometry:make-point-2d-array 12 "not a list")
  (lgeometry:not-a-list (condition)
    (format t "Error: given ~A, but expected ~A~%"
            (type-error-datum condition)
            (type-error-expected-type condition))))
