(defpackage cl-ray/tests/main
  (:use :cl
        :cl-ray
        :rove))
(in-package :cl-ray/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-ray)' in your Lisp.

(deftest test-tuple-creation
  (let ((vec (cl-ray:create-vector 4.3 -4.2 3.1)))
    (testing "(cl-ray:create-vector 4.3 -4.2 3.1)"
      (ok (cl-ray:float-equal (cl-ray:tuple-x vec) 4.3))
      (ok (cl-ray:float-equal (cl-ray:tuple-y vec) -4.2))
      (ok (cl-ray:float-equal (cl-ray:tuple-z vec) 3.1))
      (ok (cl-ray:is-vector vec))
      (ng (cl-ray:is-point vec)))))

(deftest test-point-creation
  (let ((pt (cl-ray:create-point 4.3 -4.2 3.1)))
    (testing "(cl-ray:create-point 4.3 -4.2 3.1)"
      (ok (cl-ray:float-equal (cl-ray:tuple-x pt) 4.3))
      (ok (cl-ray:float-equal (cl-ray:tuple-y pt) -4.2))
      (ok (cl-ray:float-equal (cl-ray:tuple-z pt) 3.1))
      (ng (cl-ray:is-vector pt)))
      (ok (cl-ray:is-point pt))))

(deftest test-float-equal-true
  (let ((x 4.00005)
        (y 4.000059))
    (testing "should (float-equal x y) be true"
      (ok (cl-ray:float-equal x y)))))

(deftest test-float-equal-false
  (let ((x 4.00005)
        (y 4.00006))
    (testing "should (float-equal x y) be false"
      (ng (cl-ray:float-equal x y)))))

(deftest test-tuple-addition
  (let ((t1 (cl-ray:create-point 3 -2 5))
        (t2 (cl-ray:create-vector -2 3 1))
        (t-expected (cl-ray:create-point 1 1 6)))
    (testing "(cl-ray:t+ t1 t1) should be equal to t-expected"
      (let ((result (cl-ray:t+ t1 t2)))
        (ok (cl-ray:float-equal (cl-ray:tuple-x result) (cl-ray:tuple-x t-expected)))
        (ok (cl-ray:float-equal (cl-ray:tuple-y result) (cl-ray:tuple-y t-expected)))
        (ok (cl-ray:float-equal (cl-ray:tuple-z result) (cl-ray:tuple-z t-expected)))
        (ok (cl-ray:float-equal (cl-ray:tuple-w result) (cl-ray:tuple-w t-expected)))))))