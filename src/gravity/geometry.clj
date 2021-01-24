(ns gravity.geometry)


(defn rotate [[x y] angle]
  (let [x' (- (* x (Math/cos angle)) (* y (Math/sin angle)))
        y' (+ (* x (Math/sin angle)) (* y (Math/cos angle)))]
    [x' y']))

(defn translate [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])
