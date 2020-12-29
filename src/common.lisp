;;;; Common Lisp file:
;;;;                 common.lisp
;;;;
;;;; Common utilities and macros used by the implementation
;;;;

(in-package :L-geometry)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *read-default-float-format* 'double-float))

(defmacro define-printer ((obj stream &key (type t) identity) &body body)
  `(defmethod print-object ((,obj ,obj) ,stream)
     (print-unreadable-object (,obj ,stream :type ,type :identity ,identity)
       ,@body)))
