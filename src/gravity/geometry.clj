(ns gravity.geometry)


(defn rotate 
  "Rotates a point [x y] by given angle (in radians) clockwise about origin"
  [[x y] angle]
  (let [x' (- (* x (Math/cos angle)) (* y (Math/sin angle)))
        y' (+ (* x (Math/sin angle)) (* y (Math/cos angle)))]
    [x' y']))

(defn translate 
  "Translates a point [x y] by [dx dy]"
  [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn cross-product 
  "2D cross product of vectors [u1 v1] -> [u2 v2] and [x1 y1] -> [x2 y2]"
  [[[u1 v1] [u2 v2]] [[x1 y1] [x2 y2]]]
  (let [[x' y'] [(- x2 x1) (- y2 y1)]
        [u' v'] [(- u2 u1) (- v2 v1)]]
    (- (* u' y') (* x' v'))))

(cross-product [[0 0] [1 1]] [[0 0] [3 4]])
(* (Math/sqrt 2) 5  (Math/sin (-  (Math/atan (/ 4 3)) (/ Math/PI 4)))) 

(defn direction
  "Whether bend in line between three points is to left or right or line is straight.
   Returns number < 0 if turn is to left
   Returns number > 0 if turn is to right
   Returns 0 if line is straight
  "
  [[x0 y0] [x1 y1] [x2 y2]]
  (cross-product [[0 0] [(- x2 x0) (- y2 y0)]] 
                 [[0 0] [(- x1 x0) (- y1 y0)]]))



(defn on-segment 
  "Whether point [x3 y3] lies on segment between points [x1 y1] and [x2 y2]"
  [[x1 y1] [x2 y2] [x3 y3]]
  (let [x-check (<= (min x1 x2) x3 (max x1 x2))
        y-check (<= (min y1 y2) y3 (max y1 y2))]
    (and x-check y-check)
))

(defn segments-intersect?
  "Determine if two line segments p1->p2 and p3->p4 intersect
   See SEGMENTS-INTERSECT, CLRS 3e p1018"
  [[p1 p2] [p3 p4]]
  (let [d1 (direction p3 p4 p1)
        d2 (direction p3 p4 p2)
        d3 (direction p1 p2 p3)
        d4 (direction p1 p2 p4)
        direction-check (and  (< (* d1 d2) 0) (< (* d3 d4) 0))]
    (if direction-check
      true
      (cond (and (= 0 d1) (on-segment p3 p4 p1)) true 
            (and (= 0 d2) (on-segment p3 p4 p2)) true
            (and (= 0 d3) (on-segment p1 p2 p3)) true
            (and (= 0 d4) (on-segment p1 p2 p4)) true
            :else false))))

(segments-intersect? [[0 -1] [0 1]] [[-1 0] [1 0]])
(segments-intersect? [[0 0] [1 0]] [[0 0] [0 1]])
(segments-intersect? [[1 1] [1 2]] [[1 1] [2 1]])
(segments-intersect? [[1 1] [2 1]] [[1 1] [1 2]])
(segments-intersect? [[0 0] [0 1]] [[1 1] [2 1]])
(segments-intersect? [[1 2] [3 7]] [[9 12] [6 19]])
(segments-intersect? [[0 0] [0 1]] [[0 0] [4 3]])

(direction [-1 0] [1 0] [0 -1]) ; 3,4,1
(direction [-1 0] [1 0] [0 1])  ; 3,4,2
(direction [0 -1] [0 1] [-1 0]) ; 1,2,3
(direction [0 -1] [0 1] [1 0])  ; 1,2,4 

(direction [-1 0] [1 0] [-1 -1]) ; 3,4,1
(direction [-1 0] [1 0] [1 -1])  ; 3,4,2
(direction [0 -1] [0 1] [-1 0]) ; 1,2,3
(direction [0 -1] [0 1] [1 0])  ; 1,2,4

(* (Math/sin (/ Math/PI 4)) (Math/sqrt 2))
(* 5 0.8)