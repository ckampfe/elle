(ns ckampfe.curve6
  (:require [quil.core :as q]))

(defn setup []
  (q/background 255)
  #_(q/no-stroke)
  #_(q/frame-rate 1)
  (q/no-fill)
  (q/no-loop))

(def base-points
  [[105 400]
   #_[105 400]
   [205 10]
   [500 400]
   [795 10]
   [895 400]

   #_[895 400]
   #_[795 10]
   #_[500 400]
   #_[205 10]
   #_[105 400]])

(defn draw []
  (let [transformations (take 50 (iterate (fn [prev]
                                            (+ prev (Math/sqrt prev))) 40))
        curves1 (map (fn [t]
                       (map (fn [[x y]]
                              [(+ 15 x)
                               (+ 400 (* y (/ t 950)))])
                            base-points))
                     transformations)

        curves2 (map (fn [t]
                       (map (fn [[x y]]
                              (let [ynorm (+ 400 (* y (/ t 950)))
                                    yadjust (- ynorm (* 2 (* y (/ t 950))))]
                                [(+ 15 x)
                                 yadjust
                                 ]))

                            base-points))
                     transformations)]

    (doseq [curve curves1]
      #_(q/begin-shape)
      (doseq [[p1 p2] (partition 2 1 curve)]
        #_(q/curve-tightness 1.0)
        (q/line p1 p2)

        #_(apply q/curve-vertex p1)
        #_(apply q/curve-vertex p2))
      #_(q/end-shape :close))

    (doseq [curve curves2]
      #_(q/begin-shape)
      (doseq [[p1 p2] (partition 2 1 curve)]
        #_(q/curve-tightness 1.0)

        #_(apply q/curve-vertex p1)
        #_(apply q/curve-vertex p2)

        (q/line p1 p2)
        )
      #_(q/end-shape :close))))

(try (q/defsketch example
       :title "hull"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       :renderer :svg
       :output-file "hull6_redux.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
