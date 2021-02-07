;;;; Common Lisp file:
;;;;                 conditions.lisp
;;;;

(in-package :lgeometry)

(define-condition value-error (error)
  ((%received :initarg :received
              :reader received)))

(define-condition undefined-metric (undefined-function)
  ((%available-metrics :initarg :available-metrics
                       :reader available-metrics))
  (:report (lambda (condition stream)
             (format stream "~A not defined, available metrics:~%~{~A~^, ~}~% ~
                             To define a new metric, use DEFINE-METRIC macro"
                     (cell-error-name condition)
                     (available-metrics condition)))))

(define-condition not-same-length (value-error)
  ((%listlen :initarg :listlen
             :reader listlen)
   (%othervalue :initarg :othervalue
                :reader othervalue)
   (%otherlen :initarg :otherlen
              :reader otherlen))
  (:report (lambda (condition stream)
             (format stream "~A has length ~D and ~A has length ~D. ~
                             They must have the same length"
                     (received condition)
                     (listlen condition)
                     (othervalue condition)
                     (otherlen condition)))))

(define-condition not-a-list (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~A is not of type ~A"
                     (type-error-datum condition)
                     (type-error-expected-type condition)))))
