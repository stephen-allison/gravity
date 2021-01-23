(ns gravity.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def gravity-strength -0.01)
(def thrust-strength 0.1)
(def initial-state {:ship 
                    {:heading 0 :pos [400 300] :v [0 0] :forces {:gravity [0 gravity-strength]}}
                    :platforms [[350 50 450 50]]
                    })    

(defn ship-mass [ship] 1)

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  ; setup function returns initial state.
  initial-state)

(def ship-points [-9 -6 9 -6 0 18])


;want matrix for cw rotation 
;when +ve y is down
;|1  0||c -s|    |c  -s|
;|0 -1||s  c|  = |-s -c|
;
;|c  -s||x|   | xc - ys|
;|-s -c||y| = |-xs - cy|

(defn rotate [[x y] angle]
  (let [x' (- (* x (Math/cos angle)) (* y (Math/sin angle)))
        y' (+ (* y (Math/cos angle)) (* x (Math/sin angle)))]
    [x' y'] 
))

(defn translate [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])


(defn draw-ship [ship]
  (let [rotated (map #(rotate %1 (ship :heading)) (partition 2 ship-points))
        translated (map #(translate %1 (ship :pos)) rotated)]
    (q/fill 255 255 255)
    (apply q/triangle (flatten translated))))

(defn draw-platforms [platforms]
  (doseq [line platforms]
    (q/stroke 255 255 255)           
    (apply q/line line)))
       
(defn update-with-key-input [state] 
  (if (q/key-pressed?)
    (case (q/key-as-keyword)
      :space (thrust-on state)
      :left (update-in state [:ship :heading] #(mod (+ %1 0.1) (* 2 q/PI)))            
      :right (update-in state [:ship :heading] #(mod (- %1 0.1) (* 2 q/PI)))
      :r initial-state
      (thrust-off state))
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
  (q/scale 1 -1)
  (q/translate 0 -600)
  (q/background 0 0 23)
  (draw-ship (state :ship))
  (draw-platforms (state :platforms)))

(q/defsketch gravity
  :title "Gravity"
  :size [800 600]
  :setup setup            
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])                                    

