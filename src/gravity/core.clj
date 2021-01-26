(ns gravity.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [gravity.geometry :as g]))

(def gravity-strength -0.03)
(def thrust-strength 0.1)
(def initial-state {:ship 
                    {:heading 0 
                     :pos [400 300] 
                     :v [0 0] 
                     :forces {:gravity [0 gravity-strength]}
                     :feelers []}
                    :platforms [[350 50 450 50]]
                    })    

(def ship-points [-9 -6 9 -6 0 18])

(defn ship-mass [ship] 1)

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  ; setup function returns initial state.
  initial-state)

(defn current-ship-points [ship]
  (let [rotated (map #(g/rotate %1 (ship :heading)) (partition 2 ship-points))] 
    (map #(g/translate %1 (ship :pos)) rotated)))
 
(defn draw-ship [ship]
  (let [translated (current-ship-points ship)]
    (q/fill 255 255 255)
    (apply q/triangle (flatten translated))
    (q/stroke 255 0 0)
    (q/stroke-weight 2)
    (doseq [[p1 p2] (ship :feelers)]
      (q/line p1 p2))   
))           

(defn draw-platforms [platforms]
  (doseq [line platforms]
    (q/stroke 255 255 255)           
    (apply q/line line)))
       
(defn thrust-off [state]  
  (assoc-in state [:ship :forces :thrust] [0 0])) 

(defn thrust-on [state]
  (let [heading (get-in state [:ship :heading])
        x-component (* -1 (Math/sin heading))
        y-component (Math/cos heading)
        strength thrust-strength]
    (assoc-in state [:ship :forces :thrust] [(* strength x-component) (* strength y-component)])))

(defn update-with-key-input [state] 
  (if (q/key-pressed?)
    (case (q/key-as-keyword)
      :space (thrust-on state)
      :left (update-in state [:ship :heading] #(mod (+ %1 0.1) (* 2 q/PI)))            
      :right (update-in state [:ship :heading] #(mod (- %1 0.1) (* 2 q/PI)))
      :r initial-state
      (thrust-off state))
    (thrust-off state)))
       
(defn update-position [state]
  (let [[x y] (get-in state [:ship :pos])
        [vx vy] (get-in state [:ship :v])]
    (assoc-in state [:ship :pos] [(+ x vx) (+ y vy)])))

(defn update-collision-feelers [state]
  (let [v (get-in state [:ship :v])
        pos (get-in state [:ship :pos])
        feelers (for [pt (current-ship-points (state :ship))] 
                  [pt (g/translate pt v)])]
    (assoc-in state [:ship :feelers] feelers)
))

(defn update-velocity [state]
  (let [[vx vy] (get-in state [:ship :v])
        forces (get-in state [:ship :forces])
        mass (ship-mass (state :ship))]
    (assoc-in state [:ship :v] 
              (reduce (fn [[vx vy] [fx fy]] [(+ vx (/ fx mass)) (+ vy (/ fy mass))]) 
                      [vx vy] 
                      (vals forces)))))

(defn check-collisions [state]
  (let [obstacles (get-in state [:platforms])
        lines (map #(partition 2 %1) obstacles)
        feelers (get-in state [:ship :feelers])
        colls (for [f feelers ln lines] (g/segments-intersect? f ln))]
    (when (some true? colls)
      (println (str "Collision " (vector colls)))
      state)
    state               
))

(defn update-state [state]
  (-> state
      update-velocity
      update-collision-feelers       
      update-position
      update-with-key-input          
      check-collisions
))             

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

