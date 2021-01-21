(ns gravity.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def initial-state {:ship {:heading 0 :pos [400 300] :p [1 -5] :forces [[0 0.1]]}})

(defn ship-mass [ship] 1)

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  ; setup function returns initial state.
  initial-state)

(defn draw-ship [ship]
  (let [x (first (ship :pos))
        y (second (ship :pos))]
    (q/with-translation [x y]
      (q/with-rotation [(ship :heading)]
        (q/fill 255 255 255)
        (q/triangle -9 -6 9 -6 0 18)))))
       
 (defn update-with-key-input [state] 
  (if (q/key-pressed?)
    (do
      (println (state :ship))
      (case (q/key-as-keyword)
        :left (update-in state [:ship :heading] #(- %1 0.1))
        :right (update-in state [:ship :heading]  #(+ %1 0.1))
        :else state))
    state))

(defn update-position [state]
  (let [[x y] (get-in state [:ship :pos])
        [px py] (get-in state [:ship :p])]
    (assoc-in state [:ship :pos] [(+ x px) (+ y py)])))

(defn update-momentum [state]
  (let [[px py] (get-in state [:ship :p])
        forces (get-in state [:ship :forces])
        mass (ship-mass (state :ship))]
    (assoc-in state [:ship :p] 
              (reduce (fn [[px py] [fx fy]] [(+ px (/ fx mass)) (+ py (/ fy mass))]) 
                      [px py] 
                      forces))))

(defn update-state [state]
  (-> state
      update-momentum
      update-with-key-input
      update-position))

(defn draw-state [state]
  (q/background 0 0 23)
  (draw-ship (state :ship)))

(q/defsketch gravity
  :title "Gravity"
  :size [800 600]
  :setup setup            
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])                  
