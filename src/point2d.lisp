;;;; Common Lisp file:
;;;;                 point2d.lisp
;;;;
;;;; Methods and algorithms for points in R²
;;;;

(in-package :lgeometry)

;;; TODO Use stable floating-point algorithms

(defparameter *metrics* (list #'euclidean-metric))

(defclass point-2d-cartesian ()
  ((%cartesian-2d :initarg :cartesian-2d
                  :accessor cartesian-2d
                  :type '(vector double-float)))
  (:documentation "2D Cartesian point represented by the vector #(x y)"))

(defclass point-2d-polar ()
  ((%polar-2d :initarg :polar-2d
              :accessor polar-2d
              :type '(vector double-float)))
  (:documentation "2D Polar point represented by the vector #(r theta), where
r is radius and theta is the angle in the polar representation"))

(defclass point-2d-vector ()
  ((%vector-2d :initarg :vector-2d
               :accessor vector-2d
               :type '(vector point-2d-cartesian)))
  (:documentation "Vector consisting of point-2d-cartesian"))

(define-printer (point-2d-cartesian stream :type t :identity t)
  (format stream "~A" (cartesian-2d point-2d-cartesian)))

(define-printer (point-2d-polar stream :type t :identity t)
  (format stream "~A" (polar-2d point-2d-polar)))

(define-printer (point-2d-vector stream :type t :identity t)
  (let* ((vec (vector-2d point-2d-vector))
         (len (length (vector-2d point-2d-vector)))
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
  "Computes a distance metric in 2D plane.
Parameters:
   p1 and p2 must be instances of point-2d-cartesian class
   distance-fn must be one of distance metric exported by lgeometry or one defined
by lgeometry:define-metric"
  (if (member distance-fn *metrics*)
      (let ((p1-coords (slot-value p1 '%cartesian-2d))
            (p2-coords (slot-value p2 '%cartesian-2d)))
        (funcall distance-fn p1-coords p2-coords))
      (error 'undefined-metric
             :reason (format nil "'distance-fn' must be one of ~{~A~^, ~}" *metrics*))))

(defmethod cartesian->polar ((cartesian point-2d-cartesian))
  (assert (typep cartesian 'point-2d-cartesian))
  (let* ((v (slot-value cartesian '%cartesian-2d))
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
  (let* ((v (slot-value polar '%polar-2d))
         (r (aref v 0))
         (theta (aref v 1)))
    (make-point-2d-cartesian (* r (cos theta))
                             (* r (sin theta)))))

(defun make-point-2d-array (X Y)
  "Returns a new class point-2d-vector that consists of an array of 2D cartesian points.
- X   a list of double-float values corresponding to the x coordinates of the points
- Y   a list of double-float values corresponding to the y coordinates of the points
X and Y can be two numbers too."
  (if (and (numberp X) (numberp Y))
      (progn
        (setf X (list (coerce X 'double-float)))
        (setf Y (list (coerce Y 'double-float))))
      (cond ((and (not (numberp X)) (not (listp X)))
             (error 'not-a-list :datum (type-of X) :expected-type 'list))
            ((not (listp Y))
             (error 'not-a-list :datum (type-of Y) :expected-type 'list))))
  (let ((lenx (length X)))
    (if (/= lenx (length Y))
        (error 'not-same-length :reason "'X' and 'Y' must be of the same length")
        (make-instance
         'point-2d-vector
         :vector-2d (make-array
                     lenx
                     :element-type 'point-2d-cartesian
                     :initial-contents (mapcar #'(lambda (px py)
                                                   (make-point-2d-cartesian px py))
                                               X Y))))))

(defun %lexicographic-order (point1 point2)
  (let* ((pt1 (cartesian-2d point1))
         (pt2 (cartesian-2d point2))
         (len (length pt1)))
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
      (lex-order pt1 pt2 0))))

(defun %compute-hull (points len)
  "Computes lower and upper hull as described in Andrew's algorithm for two-dimensional
convex hull:
A. M. Andrew, 'Another Efficient Algorithm for Convex Hulls in Two Dimensions', Info. Proc. Letters 9, 216-219 (1979)."
    (let ((upper-hull (make-array 1 :adjustable t :fill-pointer 0))
          (lower-hull (make-array 1 :adjustable t :fill-pointer 0))
          (line-orientation (lambda (p1 p2 p3)
                              (- (* (- (aref p2 0) (aref p1 0))
                                    (- (aref p3 1) (aref p1 1)))
                                 (* (- (aref p2 1) (aref p1 1))
                                    (- (aref p3 0) (aref p1 0)))))))
      (vector-push-extend (aref points 0) upper-hull)
      (vector-push-extend (aref points 1) upper-hull)
      (vector-push-extend (aref points (- len 1)) lower-hull)
      (vector-push-extend (aref points (- len 2)) lower-hull)
      (do ((i 2 (incf i))
           (j (- len 3) (decf j)))
          ((>= i len) (values upper-hull lower-hull))
        (vector-push-extend (aref points i) upper-hull)
        (vector-push-extend (aref points j) lower-hull)
        ;; Upper-Hull loop
        (loop :with top = (- (fill-pointer upper-hull) 1)
              :while (and (> top 1)
                          (>= (funcall line-orientation
                                       (cartesian-2d (aref upper-hull (- top 2)))
                                       (cartesian-2d (aref upper-hull (- top 1)))
                                       (cartesian-2d (aref upper-hull top)))
                              0.0d0))
              :do (setf (aref upper-hull (- top 1))
                        (vector-pop upper-hull))
              :do (decf top))
        ;; Lower-Hull loop
        (loop :with top = (- (fill-pointer lower-hull) 1)
              :while (and (> top 1)
                          (>= (funcall line-orientation
                                       (cartesian-2d (aref lower-hull (- top 2)))
                                       (cartesian-2d (aref lower-hull (- top 1)))
                                       (cartesian-2d (aref lower-hull top)))
                              0.0d0))
              :do (setf (aref lower-hull (- top 1))
                        (vector-pop lower-hull))
              :do (decf top)))))

(defmethod convex-hull ((points point-2d-vector))
  "Computes two-dimensional convex hull of a set of points. The 'points' argument must
be a representative of the class 'point-2d-vector.
Returns a set of points - corresponding to the points that are in the convex hull of the
original set - as a representative of the 'point-2d-vector class."
  (let* ((ps (sort (copy-seq (vector-2d points)) #'%lexicographic-order))
         (len (length ps)))
    (if (<= len 1)
        points
        (multiple-value-bind (upper-hull lower-hull)
            (%compute-hull ps len)
          (vector-pop upper-hull)
          (vector-pop lower-hull)
          (make-instance 'point-2d-vector
                         :vector-2d (concatenate '(vector point-2d-cartesian)
                                                 upper-hull lower-hull))))))
