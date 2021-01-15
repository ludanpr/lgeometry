;;;; Common Lisp file:
;;;;                 conditions.lisp
;;;;

(in-package :lgeometry)

(define-condition value-error (error)
  ((%reason :initarg :reason :reader reason)))

(define-condition undefined-metric (value-error) ())

(define-condition not-same-length (value-error) ())

(define-condition not-a-list (type-error) ())
