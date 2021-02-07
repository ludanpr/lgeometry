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
      (let ((*print-escape* nil))
        (write-to-string condition)))))

;;; Handling make-point-2d-array not-a-list condition
(handler-case (lgeometry:make-point-2d-array 12 "not a list")
  (lgeometry:not-a-list (condition)
    (let ((*print-escape* nil))
      (write-to-string condition))))

;;; Handling make-point-2d-array not-same-length condition
(handler-case (lgeometry:make-point-2d-array (list 12 13 15.34 533.29 21)
                                             (list 0 1 2.2 3 4.4 5))
  (lgeometry:not-same-length (condition)
    (let ((*print-escape* nil))
      (write-to-string condition))))
