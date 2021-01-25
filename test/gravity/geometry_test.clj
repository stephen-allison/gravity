(ns gravity.geometry-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gravity.geometry :as g]))


(defn within [epsilon a b]
  (< (Math/abs (- a b)) epsilon))

(def close-enough (partial within 0.000001))

(deftest test-rotation
  (testing "PI/2 rotation of (1,0)"
    (let [[x y] (g/rotate [1 0] (/ Math/PI 2))]
      (is (close-enough x 0 ))
      (is (close-enough y 1))))
  (testing "PI rotation of (1,0)"
    (let [[x y] (g/rotate [1 0] Math/PI)]
      (is (close-enough x -1))
      (is (close-enough y 0)))))

(deftest test-cross-product
  (testing "Orthogonal vectors have cross product equal to product of lengths"
    (is (= 4 (g/cross-product [[0 0] [2 0]] [[0 0] [0 2]]))))
  (testing "Co-linear vectors have zero cross product"
    (is (= 0 (g/cross-product [[0 0] [1 0]] [[5 0] [6 0]]))))
  (testing "Parallel vectors have zero cross product"
    (is (= 0 (g/cross-product [[2 2] [3 3]] [[5 5] [6 6]]))))
  (testing "[-3,7]x[2,5] = -29"
    (is (= -29 (g/cross-product [[0 0] [-3 7]] [[0 0] [2 5]]))))
  (testing "[-3,7]x[2,5] = -29 after points translated"
    (is (= -29 (g/cross-product [[1 1] [-2 8]] [[2 2] [4 7]])))))

; a x b = |a||b|sinA
(defspec test-cross-product-of-vectors-is-sine-of-angle-between-them-times-lengths 1000
  (prop/for-all [angles (gen/vector (gen/double* {:min 0 :max (* 2 Math/PI) :NaN? false}) 2)
                [len-1 len-2] (gen/vector (gen/double* {:min 0 :max 1000 :NaN? false}) 2)] 
                (let [[a b] (sort angles)
                      angle (- b a)
                      p1 (g/rotate [len-1 0] a)
                      p2 (g/rotate [len-2 0] b)
                      cp (g/cross-product [[0 0] p1] [[0 0] p2])
                      expected (* len-1 len-2 (Math/sin angle))]
                  (close-enough expected cp))))

(g/cross-product [[0 0] (g/rotate [1 0] 0.5) ] [[0 0] (g/rotate [1 0] 1.0)])
(g/rotate [2 0] 0.5)

(deftest test-direction
  (testing "Left turn gives negative result"
    (is (> 0 (g/direction [0 0] [1 1] [0 2]))))
  (testing "Right turn gives positive result"
    (is (< 0 (g/direction [0 0] [1 1] [2 0]))))
  (testing "Co-linear points gives zero"
    (is (= 0 (g/direction [0 0] [1 1] [2 2])))))

(deftest test-segments-intersect?
  (testing "x- and y- axes intersect"
    (is (true? (g/segments-intersect? [[-1 0] [1 0]] [[0 -1] [0 1]]))))
  (testing "Parallel lines don't intersect"
    (is (false? (g/segments-intersect? [[1 1] [5 1]] [[1 2] [5 2]]))))
  (testing "Overlapping co-linear lines intsersect"
    (is (true? (g/segments-intersect? [[1 1] [3 3]] [[2 2] [4 4]]))))
  (testing "Common endpoint counts as intersection"
    (is (true? (g/segments-intersect? [[0 0] [1 1]] [[0 2] [1 1]]))))
  (testing "Non-orthodonal intersecting lines"
    (is (true? (g/segments-intersect? [[-1 1] [9 3]] [[-1 8] [9 -3]]))))
  (testing "Non-orthodonal non-intersect?ing lines"
    (is (false? (g/segments-intersect? [[-1 1] [9 3]] [[-1 8] [1 3]])))))

(defspec test-overlapping-cords-intersect 1000
  (prop/for-all [angles (gen/vector-distinct (gen/double* 
                                              {:min 1E-5 
                                               :max (* 2 Math/PI) 
                                               :NaN? false })
                                              {:num-elements 4})]
                (let [[a b c d] (sort angles)
                      p1 (g/rotate [10 0] a)
                      p2 (g/rotate [10 0] b)
                      p3 (g/rotate [10 0] c)
                      p4 (g/rotate [10 0] d)]
                  (is (true? (g/segments-intersect? [p1 p3] [p2 p4])) (str [a b c d] [p1 p3] [p2 p4])))))

(defspec test-non-overlapping-cords--do-not-intersect 1000
  (prop/for-all [angles (gen/vector-distinct (gen/double* 
                                              {:min 1E-5 
                                               :max (* 2 Math/PI) 
                                               :NaN? false })
                                              {:num-elements 4})]
                (let [[a b c d] (sort angles)
                      p1 (g/rotate [10 0] a)
                      p2 (g/rotate [10 0] b)
                      p3 (g/rotate [10 0] c)
                      p4 (g/rotate [10 0] d)]
                  (is (false? (g/segments-intersect? [p1 p2] [p3 p4])) (str [a b c d] [p1 p2] [p3 p4])))))
