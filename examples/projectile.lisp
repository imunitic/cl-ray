(defpackage cl-ray/examples/projectile
  (:use :cl
	:cl-ray)
  (:export :main))

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

;; TODO (imunitic) :: Finish the implementation of the loop here
(defun main ()
  (let ((proj (make-projectile
	    :position (cl-ray:create-point 0 1 0)
	    :velocity (cl-ray:t* (cl-ray:normalize (cl-ray:create-vector 1 1 0)) 1)))
	(env (make-environment
	      :gravity (cl-ray:create-vector 0 -0.1 0)
	      :wind (cl-ray:create-vector -0.01 0 0)))
	(count 0))
    (loop while (<= 0 (tuple-y (projectile-position proj))) do
      (format t "~d :: ~a~%" count (projectile-position proj))
      (incf count)
      (setf proj (tick env proj)))))
