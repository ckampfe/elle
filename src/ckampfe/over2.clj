(ns ckampfe.over2
  (:require [quil.core :as q]))

(def width 1200)
(def height 800)

(defn setup []
  (q/background 255)
  (q/fill 0 0 0)
  (q/no-loop))

(defn tiles [w h size]
  [(quot w size)
   (quot h size)])

(defn draw []
  #_(q/curve-tightness -4.0)
  (let [[columns rows] (tiles width height 60)]
    (let [p1 [1/2 0]
          p2 [1/2 1/2]
          p3 [1/4 1]
          p4 [1 1]
          p5 [1/2 1/2]
          p6 [1/4 0]]

      (doseq [column (range columns)
              row (range (- rows 2))]
        (let [a1 [(+ (* (first p1) 60) (* column 60))
                  (+ (* (second p1) 60) (* row 60))]

              a2 [(+ (* (first p2) 60) (* column 60))
                  (+ (* (second p2) 60) (* row 60))]

              a3 [(+ (* (first p3) 60) (* column 60))
                  (+ (* (second p3) 60) (* row 60))]

              a4 [(+ (* (first p4) 60) (* column 60))
                  (+ (* (second p4) 60) (* row 60))]

              a5 [(+ (* (first p5) 60) (* column 60))
                  (+ (* (second p5) 60) (* row 60))]

              a6 [(+ (* (first p6) 60) (* column 60))
                  (+ (* (second p6) 60) (* row 60))]

              ]

          (q/begin-shape)
          (apply q/curve-vertex a1)
          (apply q/curve-vertex a2)
          (apply q/curve-vertex a3)
          (apply q/curve-vertex a4)
          (apply q/curve-vertex a5)
          (apply q/curve-vertex a6)
          (apply q/curve-vertex a1)
          (q/end-shape :close))))))

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
