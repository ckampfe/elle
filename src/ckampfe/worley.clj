(ns ckampfe.worley
  (:require [quil.core :as q]
            [clojure.core.matrix :as m]
            [clojure.core.reducers :as r]))

(def width 1200)

(def height 800)

(defn gen-points [n width height]
  (map (fn [_]
         [(rand-int width)
          (rand-int height)])
       (range n)))

#_(gen-points 10 width height)

#_(q/dist)

;; {[x y] -> [float ...] ...}
(defn dist-map [points]
  (let [xs (range width)
        ys (range height)
        kv (time (pmap
                  (fn [[x1 y1]]
                    [[x1 y1]
                     (take 5 (sort (for [[x2 y2] points]
                                     (q/dist x1 y1 x2 y2))))])
                  (for [x xs
                        y ys]
                    [x y])))]
    (time (into {} kv))))

(defn setup []
  (q/background 255)
  (q/no-loop))

(defn draw []
  (let [points (gen-points 40 width height)
        pixel-distances (dist-map points)
        pixels (q/pixels)]

    (time (doseq [[[x y] distances] pixel-distances]
            (let [v (first distances)]
              (aset pixels
                    (+ x (* y width))
                    #_(q/color (rem v 255)
                               (rem v 155)
                               (rem v 100))
                    (q/color (* 1.0 v)
                             (* 0.1 v)
                             (* 0.6 v)))
              #_(q/set-pixel x
                             y
                             #_(q/color (mod v 255)
                                        (mod v 155)
                                        (mod v 100))
                      ;; green/teal
                             #_(q/color (* 0.3 v)
                                        (* 1.0 v)
                                        (* 0.7 v))
                      ;; pinkish
                             (q/color (* 1.0 v)
                                      (* 0.1 v)
                                      (* 0.6 v))))))
    (q/update-pixels)
    (q/save "worley.png")
    ))

(try (q/defsketch example
       :title "worley"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :java2d
       ;; :output-file "worley.png"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
