;;;; Common Lisp file:
;;;;                 package.lisp
;;;;

(in-package :cl-user)

(defpackage #:lgeometry
  (:nicknames #:lgeo
              #:lgeom)
  (:use #:common-lisp
        #:bordeaux-threads)
  (:export #:make-point-2d-cartesian
           #:make-point-2d-polar
           ;;----------------------------------------------------------------
           ;; Exported for its function designators only, not for direct call
           #:euclidean-metric
           ;;----------------------------------------------------------------
           #:distance-metric
           #:define-metric
           #:cartesian->polar
           #:polar->cartesian
           #:make-point-2d-array
           #:convex-hull
           ;;----------------------------------
           ;; Exported Conditions
           ;;----------------------------------
           #:value-error
           #:undefined-metric
           #:not-same-length
           #:not-a-list
           ;;----------------------------------
           ;; Condition readers
           ;;----------------------------------
           #:reason))
