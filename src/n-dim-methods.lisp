;;;; Common Lisp file:
;;;;                 n-dim-methods.lisp
;;;;
;;;; Implements auxiliary common methods that can be used in n-dimensional space
;;;;

(in-package :L-geometry)

;;;-----------------------------
;;; Generic functions
;;;-----------------------------

(defgeneric distance-metric (p1 p2 &optional distance-fn)
  (:documentation
   "Computes a distance metric given by the functional argument distance-fn.
Computes euclidean distance by default."))

(defgeneric polar->cartesian (polar))

(defgeneric cartesian->polar (cartesian))

(defgeneric convex-hull (points))

;;;-----------------------------

(defun euclidean-metric (p1 p2)
  "Compute Euclidean distance in R^n"
  (sqrt (reduce #'+
                (map 'list #'(lambda (x1 x2)
                               (expt (- x1 x2) 2))
                     p1 p2))))
