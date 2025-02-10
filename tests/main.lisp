(defpackage cl-ray/tests/main
  (:use :cl
        :cl-ray
        :rove))
(in-package :cl-ray/tests/main)

;; helpers 
(defun check-tuple-equality (result expected)
  (ok (cl-ray:float-equal (cl-ray:tuple-x result) (cl-ray:tuple-x expected)))
  (ok (cl-ray:float-equal (cl-ray:tuple-y result) (cl-ray:tuple-y expected)))
  (ok (cl-ray:float-equal (cl-ray:tuple-z result) (cl-ray:tuple-z expected)))
  (ok (cl-ray:float-equal (cl-ray:tuple-w result) (cl-ray:tuple-w expected))))

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
  (let* ((t1 (cl-ray:create-point 3 -2 5))
        (t2 (cl-ray:create-vector -2 3 1))
        (t-expected (cl-ray:create-point 1 1 6))
        (result (cl-ray:t+ t1 t2)))
    (testing "(cl-ray:t+ t1 t2) should be equal to t-expected"
      (check-tuple-equality result t-expected))))

(deftest test-tuple-subtraction
  (let* ((t1 (cl-ray:create-vector 3 2 1))
        (t2 (cl-ray:create-vector 5 6 7))
        (t-expected (cl-ray:create-vector -2 -4 -6))
        (result (cl-ray:t- t1 t2)))
    (testing "(cl-ray:t- t1 t2) should be equal to t-expected"
      (check-tuple-equality result t-expected))))

(deftest test-vector-negation
  (let* ((t1 (cl-ray:create-vector 1 -2 3))
        (t-expected (cl-ray:create-vector -1 2 -3))
        (result (cl-ray:t-neg t1)))
    (testing "(cl-ray:t-neg t1) should be equal to t-expected"
      (check-tuple-equality result t-expected))))

(deftest test-tuple-negation 
  (let* ((t1 (cl-ray:create-tuple 1 -2 3 -4))
        (t-expected (cl-ray:create-tuple -1 2 -3 4))
        (result (cl-ray:t-neg t1)))
    (testing "(cl-ray:t-neg t1) should be equal to t-expected" 
      (check-tuple-equality result t-expected))))

(deftest test-tuple-multiplication
  (let* ((t1 (cl-ray:create-tuple 1 -2 3 -4))
        (t-expected (cl-ray:create-tuple 3.5 -7 10.5 -14))
        (result (cl-ray:t* t1 3.5)))
    (testing "(cl-ray:t* t1 3.5) should be equal to t-expected"
      (check-tuple-equality result t-expected))))

(deftest test-tuple-multiplication-fraction
  (let* ((t1 (cl-ray:create-tuple 1 -2 3 -4))
        (t-expected (cl-ray:create-tuple 0.5 -1 1.5 -2))
        (result (cl-ray:t* t1 0.5)))
    (testing "(cl-ray:t* t1 0.5) should be equal to t-expected"
      (check-tuple-equality result t-expected))))

