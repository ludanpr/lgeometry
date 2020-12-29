;;;; Common Lisp file:
;;;;                 package.lisp
;;;;

(in-package :cl-user)

(defpackage #:L-geometry
  (:nicknames #:L-geo
              #:lgeometry)
  (:use #:common-lisp
        #:bordeaux-threads)
  (:export #:make-point-2d-cartesian
           #:make-point-2d-polar
           #:euclidean-metric
           #:distance-metric
           #:cartesian->polar
           #:polar->cartesian
           #:make-point-2d-array
           #:convex-hull))
