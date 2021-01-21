(ns gravity.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def gravity-strength 0.03)
(def thrust-strength 0.1)
(def initial-state {:ship {:heading q/PI :pos [400 300] :v [0 0] :forces {:gravity [0 gravity-strength]}}})

(defn ship-mass [ship] 1)

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  ; setup function returns initial state.
  initial-state)

(defn draw-ship [ship]
  (let [[x y] (ship :pos)]
    (q/with-translation [x y]
      (q/with-rotation [(ship :heading)]
        (q/fill 255 255 255)
        (q/triangle -9 -6 9 -6 0 18)))))
       
 (defn update-with-key-input [state] 
  (if (q/key-pressed?)
    (do
      (println (state :ship))
      (case (q/key-as-keyword)
        :space (thrust-on state)
        :left (update-in state [:ship :heading] #(mod (- %1 0.1) (* 2 q/PI)))            
        :right (update-in state [:ship :heading] #(mod (+ %1 0.1) (* 2 q/PI)))
        (thrust-off state)))
    (thrust-off state))) 

(defn thrust-off [state]
  (assoc-in state [:ship :forces :thrust] [0 0]))

(defn thrust-on [state]
  (let [heading (get-in state [:ship :heading])
        x-component (* -1 (Math/sin heading))
        y-component (Math/cos heading)
        strength thrust-strength]
    (assoc-in state [:ship :forces :thrust] [(* strength x-component) (* strength y-component)])))
       
(defn update-position [state]
  (let [[x y] (get-in state [:ship :pos])
        [vx vy] (get-in state [:ship :v])]
    (assoc-in state [:ship :pos] [(+ x vx) (+ y vy)])))

(defn update-velocity [state]
  (let [[vx vy] (get-in state [:ship :v])
        forces (get-in state [:ship :forces])
        mass (ship-mass (state :ship))]
    (assoc-in state [:ship :v] 
              (reduce (fn [[vx vy] [fx fy]] [(+ vx (/ fx mass)) (+ vy (/ fy mass))]) 
                      [vx vy] 
                      (vals forces)))))

(defn update-state [state]
  (-> state
      update-velocity
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
