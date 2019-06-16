(ns ckampfe.curve1
  (:require [quil.core :as q]))

(defn setup []
  (q/background 255)
  #_(q/no-stroke)
  #_(q/frame-rate 1)
  (q/no-fill)
  (q/no-loop))

(def base-points
  [[20 30]
   [70 35]
   [105 80]
   [205 10]
   [300 90]
   [500 400]
   ])

(defn draw []
  (let [transformations (take 30 (iterate (fn [prev]
                                            (+ prev (Math/sqrt prev))) 40))
        curves (map (fn [t]
                      (map (fn [[x y]]
                             [(+ x t) (+ t y)])
                           base-points))
                    transformations)]

    (q/begin-shape)
    (doseq [curve curves]
      (doseq [point curve]
        (q/curve-tightness 1.1)
        (apply q/curve-vertex point)
        ))
    (q/end-shape :close)
    )

  )

(try (q/defsketch example
       :title "hull"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       :renderer :svg
       :output-file "hull1.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
