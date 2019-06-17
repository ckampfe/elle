(ns ckampfe.curve7
  (:require [quil.core :as q]))

(defn setup []
  (q/background 255)
  #_(q/no-stroke)
  #_(q/frame-rate 1)
  (q/no-fill)
  (q/no-loop))

(def base-points
  [[20 20]
   [105 400]
   [205 10]
   [500 200]
   [520 220]])

(defn draw []
  (let [transformations (take 30 (iterate (fn [prev] (+ prev  (Math/sqrt (Math/abs prev))))
                                          -40))
        curves1 (map (fn [t]
                       (map (fn [[x y]]
                              [(+ 15 (* (Math/abs (Math/atan (* t x))) x))
                               (+ 300 (* y (/ t 100)))])
                            base-points))
                     transformations)]

    (doseq [curve curves1]
      (q/begin-shape)
      (q/curve-tightness 0.1)
      (doseq [point curve]
        #_(println point)
        (apply q/curve-vertex point))
      (q/end-shape :close))))

(try (q/defsketch example
       :title "hull"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :svg
       ;; :output-file "hull7_curved.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
