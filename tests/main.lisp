(defpackage cl-ciph/tests/main
  (:use :cl
        :cl-ciph
        :rove))
(in-package :cl-ciph/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-ciph)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
