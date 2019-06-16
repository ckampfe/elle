(ns ckampfe.curve4
  (:require [quil.core :as q]))

(defn setup []
  (q/background 255)
  #_(q/no-stroke)
  #_(q/frame-rate 1)
  (q/no-fill)
  (q/no-loop))

(def base-points
  [[105 400]
   [205 10]
   [500 400]
   [795 10]
   [895 400]])

(defn draw []
  (let [transformations (take 37 (iterate (fn [prev]
                                            (+ prev (Math/sqrt prev))) 40))
        curves (map (fn [t]
                      (map (fn [[x y]]
                             [(+ 15 x)
                              (+ 15 (* y (/ t 800)))])
                           base-points))
                    transformations)]

    (q/begin-shape)
    (doseq [curve curves]
      (doseq [point curve]
        (q/curve-tightness 1.1)
        (apply q/curve-vertex point)))
    (q/end-shape :close)))

(try (q/defsketch example
       :title "hull"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       :renderer :svg
       :output-file "hull4.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
