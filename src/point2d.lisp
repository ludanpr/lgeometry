;;;; Common Lisp file:
;;;;                 point2d.lisp
;;;;
;;;; Methods and algorithms for points in R²
;;;;

(in-package :L-geometry)

;;; TODO maybe tune computations for precision

(defparameter *metrics* (list #'euclidean-metric))

(defclass point-2d-cartesian ()
  ((cartesian-2d :initarg :cartesian-2d
                 :accessor cartesian-2d
                 :type '(vector double-float)))
  (:documentation "2D Cartesian point represented by the vector #(x y)"))

(defclass point-2d-polar ()
  ((polar-2d :initarg :polar-2d
             :accessor polar-2d
             :type '(vector double-float)))
  (:documentation "2D Polar point represented by the vector #(r theta), where
r is radius and theta is the angle in the polar representation"))

(defclass point-vector-2d ()
  ((vector-2d :initarg :vector-2d
              :accessor vector-2d
              :type '(vector point-2d-cartesian)))
   (:documentation "Vector consisting of point-2d-cartesian"))

;(deftype point-vector-2d () '(vector point-2d-cartesian)) 

(define-printer (point-2d-cartesian stream :type t :identity t)
  (format stream "~A" (cartesian-2d point-2d-cartesian)))

(define-printer (point-2d-polar stream :type t :identity t)
  (format stream "~A" (polar-2d point-2d-polar)))

(define-printer (point-vector-2d stream :type t :identity t)
  (let* ((vec (vector-2d point-vector-2d))
         (len (length (vector-2d point-vector-2d)))
         (end len))
    (when (> end 5)
      (setf end 5))
    (map nil #'(lambda (v)
                 (format stream "~%~A" v))
         (subseq vec 0 end))
    (when (> len 5)
      (format stream " ..."))))

(defun make-point-2d-cartesian (x y)
  (assert (and (numberp x) (numberp y)))
  (setf x (coerce x 'double-float))
  (setf y (coerce y 'double-float))
  (make-instance 'point-2d-cartesian
                 :cartesian-2d (make-array 2 :element-type 'double-float
                                             :initial-contents (list x y))))

(defun make-point-2d-polar (r theta)
  (assert (and (numberp r) (numberp theta)))
  (setf r (coerce r 'double-float))
  (setf theta (coerce theta 'double-float))
  (make-instance 'point-2d-polar
                 :polar-2d (make-array 2 :element-type 'double-float
                                         :initial-contents (list r theta))))

(defmethod distance-metric ((p1 point-2d-cartesian)
                            (p2 point-2d-cartesian)
                            &optional (distance-fn #'euclidean-metric))
  (if (member distance-fn *metrics*)
      (let ((p1-coords (slot-value p1 'cartesian-2d))
            (p2-coords (slot-value p2 'cartesian-2d)))
        (funcall distance-fn p1-coords p2-coords))
      (error "[VALUE-ERROR]: 'distance-fn' must be one of ~{~A~^, ~}" *metrics*))) ;; TODO change to use condition system

(defmethod cartesian->polar ((cartesian point-2d-cartesian))
  (assert (typep cartesian 'point-2d-cartesian))
  (let* ((v (slot-value cartesian 'cartesian-2d))
         (x (aref v 0))
         (y (aref v 1))
         (phi 0.0d0))
    ;; Account for quadrant position
    (cond ((or (and (minusp x) (plusp y))
               (and (minusp x) (minusp y)))
           (setf phi pi))
          ((and (plusp x) (minusp y))
           (setf phi (* 2.0d0 pi))))
    (make-point-2d-polar (sqrt (+ (* x x) (* y y)))
                         (if (zerop x)
                             0.0d0
                             (+ (atan (/ y x)) phi)))))

(defmethod polar->cartesian ((polar point-2d-polar))
  (assert (typep polar 'point-2d-polar))
  (let* ((v (slot-value polar 'polar-2d))
         (r (aref v 0))
         (theta (aref v 1)))
    (make-point-2d-cartesian (* r (cos theta))
                             (* r (sin theta)))))

;;; TODO Common Lisp condition system
(defun make-point-2d-array (X Y)
  "Returns a new class point-vector-2d that consists of an array of 2D cartesian points.
- X   a list of double-float values corresponding to the x coordinates of the points
- Y   a list of double-float values corresponding to the y coordinates of the points
X and Y can be two numbers too."
  (if (and (numberp X) (numberp Y))
      (progn
        (setf X (list (coerce X 'double-float)))
        (setf Y (list (coerce Y 'double-float))))
      (if (not (and (listp X) (listp Y)))
          (error "[TYPE-ERROR]: 'X' and 'Y' must be lists")))
  (let ((lenx (length X))
        (cartesian-points nil))
    (cond
      ((/= lenx (length Y))
       (error "[VALUE-ERROR]: 'X' and 'Y' must be of the same length"))
      ((some #'(lambda (px py)
                 (setf cartesian-points
                       (nconc cartesian-points
                              (list (make-point-2d-cartesian px py))))
                 (or (not (typep px 'double-float))
                     (not (typep py 'double-float))))
             X Y)
       (error "[TYPE-ERROR]: 'X' and 'Y' must be lists of elements of type double-float"))
      (t
       (make-instance 'point-vector-2d
                      :vector-2d
                      (make-array lenx
                                  :element-type 'point-2d-cartesian
                                  :initial-contents cartesian-points))))))

(defun %lexicographic-order (point1 point2)
  (let ((len (length point1)))
    (labels ((lex-order (p1 p2 idx)
               (if (= idx len)
                   nil
                   (cond ((< (aref p1 idx)
                             (aref p2 idx))
                          t)
                         ((> (aref p1 idx)
                             (aref p2 idx))
                          nil)
                         (t
                          (lex-order p1 p2 (+ idx 1)))))))
      (lex-order point1 point2 0))))

"""(defmethod convex-hull ((points point-vector-2d))
  (let ((ps (sort (copy-seq (vector-2d points) #'))))
    (if bt:*supports-threads-p*)))
"""
