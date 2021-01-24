(ns gravity.geometry-test
  (:require [clojure.test :refer :all]
            [gravity.geometry :as g]))

(defn within [epsilon a b]
  (< (Math/abs (- a b)) epsilon))

(def close-enough (partial within 0.000001))

(close-enough 0.0004 0.0004001)

(deftest test-rotation
  (testing "PI/2 rotation of (1,0)"
    (let [[x y] (g/rotate [1 0] (/ Math/PI 2))]
      (is (close-enough x 0 ))
      (is (close-enough y 1))))
  (testing "PI rotation of (1,0)"
    (let [[x y] (g/rotate [1 0] Math/PI)]
      (is (close-enough x -1))
      (is (close-enough y 0)))))
