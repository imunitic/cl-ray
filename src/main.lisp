(uiop:define-package cl-ray
  (:use #:cl)
  (:export #:create-vector
           #:create-point
           #:create-tuple
           #:float-equal
           #:tuple-x
           #:tuple-y
           #:tuple-z
           #:tuple-w
           #:is-vector
           #:is-point
           #:t+
           #:t-
           #:t*
           #:t-neg
           #:magnitude
	   #:normalize
	   #:dot
	   #:cross-product))
(in-package #:cl-ray)

(defstruct tuple
  x
  y
  z
  w)

(defun create-vector (x y z)
  (make-tuple :x x :y y :z z :w 0.0))

(defun create-point (x y z)
  (make-tuple :x x :y y :z z :w 1.0))

(defun create-tuple (x y z w)
  (make-tuple :x x :y y :z z :w w))

(defun is-vector (x)
  (float-equal (tuple-w x) 0.0))

(defun is-point (x)
  (float-equal (tuple-w x) 1.0))

(defconstant +EPSILON+ 0.00001 "used for floating number comparison")

(defun float-equal (x y)
  (< (abs (- x y)) +EPSILON+))

(defun t+ (t1 t2)
  (create-tuple
    (+ (tuple-x t1) (tuple-x t2))
    (+ (tuple-y t1) (tuple-y t2))
    (+ (tuple-z t1) (tuple-z t2))
    (+ (tuple-w t1) (tuple-w t2))))

(defun t- (t1 t2)
  (create-tuple
    (- (tuple-x t1) (tuple-x t2))
    (- (tuple-y t1) (tuple-y t2))
    (- (tuple-z t1) (tuple-z t2))
    (- (tuple-w t1) (tuple-w t2))))

(defun t* (t1 scalar)
  (create-tuple
    (* (tuple-x t1) scalar)
    (* (tuple-y t1) scalar)
    (* (tuple-z t1) scalar)
    (* (tuple-w t1) scalar)))

(defun t-neg (t1)
  (let ((zero (create-tuple 0 0 0 0)))
    (t- zero t1)))

(defun magnitude (t1)
  (sqrt (+ (expt (tuple-x t1) 2)
           (expt (tuple-y t1) 2)
           (expt (tuple-z t1) 2)
           (expt (tuple-w t1) 2))))

(defun normalize (v1)
  (create-tuple
   (/ (tuple-x v1) (magnitude v1))
   (/ (tuple-y v1) (magnitude v1))
   (/ (tuple-z v1) (magnitude v1))
   (/ (tuple-w v1) (magnitude v1))))

(defun dot (v1 v2)
  (+ (* (tuple-x v1) (tuple-x v2))
     (* (tuple-y v1) (tuple-y v2))
     (* (tuple-z v1) (tuple-z v2))
     (* (tuple-w v1) (tuple-w v2))))

(defun cross-product (v1 v2)
  (cl-ray:create-vector
   (- (* (tuple-y v1) (tuple-z v2))
      (* (tuple-z v1) (tuple-y v2)))

   (- (* (tuple-z v1) (tuple-x v2))
      (* (tuple-x v1) (tuple-z v2)))

   (- (* (tuple-x v1) (tuple-y v2))
      (* (tuple-y v1) (tuple-x v2)))))
