(ns ckampfe.over1
  (:require [quil.core :as q]))

(def width 1200)
(def height 800)

(def center [600 400])

(defn setup []
  (q/background 255)
  (q/no-fill)
  (q/no-loop))

(defn move [x y angle distance]
  [(+ x (* (Math/cos angle) distance))
   (+ y (* (Math/sin angle) distance))])

(defn random-points-for-centroid [[x y]]
  (let [a1 (rand 120)
        d1 (+ 150 (rand 50))
        p1 (move x y a1 d1)

        a2 (+ 120 (rand 120))
        d2 (+ 150 (rand 25))
        p2 (move x y a2 d2)

        xs (reduce + (map first [p1 p2]))
        ys (reduce + (map first [p1 p2]))
        x3 (* x 3)
        y3 (* y 3)
        remx (- x3 xs)
        remy (- y3 ys)]

    [p1 p2 [remx remy]]))

(defn draw []
  (let [[cx cy] center
        cy (+ cy 100)
        a1 (rand 120)
        d1 (+ 50 (rand 120))
        centroid1 (move cx cy a1 d1)

        a2 (+ 120 (rand 120))
        d2 (+ 50 (rand 120))
        centroid2 (move cx cy a2 d2)

        a3 (+ 240 (rand 120))
        d3 (+ 50 (rand 120))
        centroid3 (move cx cy a3 d3)]

    (doseq [p [centroid1 centroid2 centroid3]]
      (let [points (random-points-for-centroid p)]
        (q/begin-shape)
        (doseq [point points]
          (apply q/vertex point))
        (q/end-shape :close)))))

(try (q/defsketch example
       :title "pendulums"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [width height]
       ;; :renderer :svg
       ;; :output-file "over2.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
