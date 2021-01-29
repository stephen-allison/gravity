(ns gravity.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [gravity.geometry :as g]))

(def gravity-strength -0.03)

(def thrust-strength 0.1)

(def initial-ship-state {:type :ship
                         :heading 0 
                         :pos [400 300] 
                         :v [0 0] 
                         :forces {:gravity [0 gravity-strength]}
                         :feelers []
                         :raw-points [{:name :top :point [0 18]}
                                      {:name :bl  :point [-9 -6]}
                                      {:name :br  :point [9 -6]}]})

(def initial-level-state {:terrain [{:type :platform :name :platform-1 :points [350 50 450 50]}
                                    {:type :platform :name :platform-2 :points [50 120 150 120]}]})

(def initial-state {:ship initial-ship-state
                    :level initial-level-state})    

(defn ship-points [ship]
  (ship :raw-points))

(defn ship-mass [ship] 1)

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :rgb)
  ; setup function returns initial state.
  initial-state)

(defn dispatch-collision-points [item]
  (item :type))

(defmulti collision-points dispatch-collision-points)

(defmethod collision-points :ship [ship]
  (ship :feelers))

(defmethod collision-points :platform [platform]
  [{:name (platform :name) :points (partition 2 (platform :points))}])
  
(collision-points {:type :platform, :points [350 50 450 50]})

(defn current-ship-points [ship]
  (let [rotated (map (fn [pt] {:name (:name pt) 
                               :point (g/rotate (:point pt) (ship :heading))}) 
                     (ship-points ship))] 
    (map (fn [pt] {:name (:name pt) 
                   :point (g/translate (:point pt) (ship :pos))}) rotated)))
 
(defn draw-ship [ship]
  (let [translated (map :point (current-ship-points ship))]
    (q/fill 255 255 255)
    (apply q/triangle (flatten translated))
    (q/stroke 255 0 0)
    (q/stroke-weight 2)
    (doseq [[p1 p2] (map :points (ship :feelers))]
      (q/line p1 p2))   
))           

(defn draw-platforms [terrain]
  (doseq [item terrain]
    (q/stroke 255 255 255)           
    (apply q/line (:points item))))
       
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
                  (assoc pt :points [(:point pt) (g/translate (:point pt) v)]))]
    (assoc-in state [:ship :feelers] (into [] feelers))
))

(defn update-velocity [state]
  (let [[vx vy] (get-in state [:ship :v])
        forces (get-in state [:ship :forces])
        mass (ship-mass (state :ship))]
    (assoc-in state [:ship :v] 
              (reduce (fn [[vx vy] [fx fy]] [(+ vx (/ fx mass)) (+ vy (/ fy mass))]) 
                      [vx vy] 
                      (vals forces)))))
        
(defn collision? [item-1 item-2]
  (let [pts1 (collision-points item-1) 
        pts2 (collision-points item-2)
        collisions (for [p1 pts1 p2 pts2 
                         :when (g/segments-intersect (:points p1) (:points p2))]
                     [(:name p1) (:name  p2)])]
    (if (not (empty? collisions)) (println collisions) nil)))
  

(defn check-collisions [state]
  (let [terrain (get-in state [:level :terrain])
        colls (map #(collision? (state :ship) %1) terrain)]
    (assoc state :collisions (into [] colls))
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
  (draw-platforms (get-in state [:level :terrain])))

(q/defsketch gravity
  :title "Gravity"
  :size [800 600]
  :setup setup            
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])                                    

