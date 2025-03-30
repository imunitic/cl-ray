(defpackage cl-ray/examples/projectile
  (:use :cl
	:cl-ray))

(in-package :cl-ray/examples/projectile)

(defstruct projectile
  position
  velocity)

(defstruct environment
  gravity
  wind)

(defun tick (env proj)
  (make-projectile
   :position
   (cl-ray:t+ (projectile-position proj)
	      (projectile-velocity proj))
   :velocity
   (cl-ray:t+ (projectile-velocity proj)
	      (cl-ray:t+ (environment-gravity env)
			 (environment-wind env)))))

(defvar *initial-projectile* (make-projectile
			      :position (cl-ray:create-point 0 1 0)
			      :velocity (cl-ray:normalize (cl-ray:create-vector 1 1 0))))

(defvar *initial-environment* (make-environment
			       :gravity (cl-ray:create-vector 0 -0.1 0)
			       :wind (cl-ray:create-vector -0.01 0 0)))

;; TODO (imunitic) :: Finish the implementation of the loop here
