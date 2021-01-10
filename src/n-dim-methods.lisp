;;;; Common Lisp file:
;;;;                 n-dim-methods.lisp
;;;;
;;;; Implements auxiliary common methods that can be used in n-dimensional space
;;;;

(in-package :lgeometry)

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
  "Compute Euclidean distance in R^n
NOTE: This is exported only for its function designator, it should not be
called directly. Use lgeometry:distance-metric instead with this as the
metric function argument."
  (sqrt (reduce #'+
                (map 'list #'(lambda (x1 x2)
                               (expt (- x1 x2) 2))
                     p1 p2))))

(defmacro define-metric (name (&rest params) &body body)
  "Define a new distance metric.
Use this macro to define a custom distance metric with the form:
   (define-metric metric-name (p1 p2) ...)
Use it as the distance-fn argument to lgeometry:distance-metric"
  `(progn
     (defun ,name (,@params)
       ,@body)
     (setf *metrics* (append (list #',name) *metrics*))))
