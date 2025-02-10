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
           #:t-neg))
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
  (make-tuple 
    :x (+ (tuple-x t1) (tuple-x t2))
    :y (+ (tuple-y t1) (tuple-y t2))
    :z (+ (tuple-z t1) (tuple-z t2))
    :w (+ (tuple-w t1) (tuple-w t2))))

(defun t- (t1 t2)
  (make-tuple 
    :x (- (tuple-x t1) (tuple-x t2))
    :y (- (tuple-y t1) (tuple-y t2))
    :z (- (tuple-z t1) (tuple-z t2))
    :w (- (tuple-w t1) (tuple-w t2))))

(defun t-neg (t1)
  (let ((zero (create-tuple 0 0 0 0)))
    (t- zero t1)))