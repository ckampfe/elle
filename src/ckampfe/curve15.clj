(ns ckampfe.curve15
  (:require [quil.core :as q]
            [clojure.core.matrix :as m]))

(defn setup []
  (q/background 255)
  (q/no-fill)
  (q/no-loop))

#_(def theta (Math/toRadians 30))

(defn centroid [x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6]
  [(/ (+ x1 x2 x3 x4 x5 x6) 6)
   (/ (+ y1 y2 y3 y4 y5 y6) 6)])

(defn rotate-and-scale [theta s x y px py]
  (let [theta (Math/toRadians theta)

        scale (m/matrix [[s 0 0]
                         [0 s 0]
                         [0 0 1]])

        rot (m/matrix [[(Math/cos theta) (* -1 (Math/sin theta)) 0]
                       [(Math/sin theta) (Math/cos theta) 0]
                       [0 0 1]])

        mm (m/mmul
            (m/matrix [[1 0 px]
                       [0 1 py]
                       [0 0 1]])

            rot

            scale

            (m/matrix [[1 0 (* -1 px)]
                       [0 1 (* -1 py)]
                       [0 0 1]])

            (m/matrix [x y 1]))
        [xx yy _] mm]
    [xx yy]))

(def width 1200)
(def height 800)

(defn plus-sqrt [n]
  (+ n (Math/sqrt n)))

(defn overlapping-triangles [n]
  (take n (iterate (fn [[x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6]]
                     (let [[cx cy] (centroid x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6)
                           scaling-factor (/ (+ (* 0.45 (Math/sqrt x1)) x1)
                                             x1)]
                       (flatten [
                                 (rotate-and-scale 31 scaling-factor x1 y1 cx cy)
                                 (rotate-and-scale 35 scaling-factor x2 y2 cx cy)
                                 (rotate-and-scale 31 scaling-factor x3 y3 cx cy)
                                 (rotate-and-scale 35 scaling-factor x4 y4 cx cy)
                                 (rotate-and-scale 31 scaling-factor x5 y5 cx cy)
                                 (rotate-and-scale 35 scaling-factor x6 y6 cx cy)

                                 ])))

                   [
                    (+ (/ width 2) 20) (+ (/ height 2) 10)
                    (+ (/ width 2) 37) (+ (/ height 2) 17)
                    (+ (/ width 2) 20) (+ (/ height 2) 21)

                    (- (/ width 2) 20) (+ (/ height 2) 13)
                    (- (/ width 2) 37) (+ (/ height 2) 17)
                    (- (/ width 2) 20) (+ (/ height 2) 21)
                    ]

                   #_[(- (/ width 2) 10) (- (/ height 2) 12)
                    (+ (/ width 2) 10) (+ (/ height 2) 40)
                    (- (/ width 2) 64) (+ (/ height 2) 10)

                    (+ (/ width 2) 10) (+ (/ height 2) 12)
                    (- (/ width 2) 10) (- (/ height 2) 40)
                    (+ (/ width 2) 64) (- (/ height 2) 10)

                    ])))

(defn draw []
  (let [triangles (overlapping-triangles 82)]

    (q/begin-shape)
    (doseq [[x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6] triangles]

      #_(q/line x1 y1 x2 y2)
      #_(q/line x2 y2 x3 y3)
      #_(q/line x1 y1 x3 y3)

      (q/curve-vertex x1 y1)
      (q/curve-vertex x2 y2)
      (q/curve-vertex x3 y3)
      (q/curve-vertex x4 y4)
      (q/curve-vertex x5 y5)
      (q/curve-vertex x6 y6)
      )
    (q/end-shape)))

(try (q/defsketch example
       :title "hull"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       :renderer :svg
       :output-file "rot8.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
