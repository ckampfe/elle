(ns ckampfe.worley2
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

(defn setup []
  (q/background 255)
  (q/no-loop))

(defn point-from-point-with-angle
  "angle theta is in radians"
  [x y length theta]
  {:x (+ x (* length (Math/cos theta)))
   :y (+ y (* length (Math/sin theta)))
   :length length})

(defn points-generation
  ([init-fn length-fn] (points-generation init-fn length-fn []))
  ([init-fn length-fn previous-points]
   (if (empty? previous-points)
     (let [{[x y] :centroid
            npoints :n-points} (init-fn)
           points (map (fn [n]
                         (let [length (length-fn) #_(+ (rand-int 70) 100)
                               angle (* (/ n npoints) (* Math/PI 2))]
                           (point-from-point-with-angle x y length angle)))
                       (range npoints))]
       {:centroid [x y]
        :n-points npoints
        :points points})
     (let [{[cx cy] :centroid
            npoints :n-points
            points :points} previous-points
           new-points (map-indexed (fn [n {x :x
                                           y :y
                                           old-length :length}]
                                     (let [new-length (length-fn old-length)
                                           angle (* (/ n npoints) (* Math/PI 2))]
                                       (point-from-point-with-angle cx cy new-length angle)))
                                   points)]
       {:centroid [cx cy]
        :n-points npoints
        :points new-points}))))

(defn draw []
  (q/no-fill)
  (let [[x1 y1] [(/ width 3) (/ height 2)]
        [x2 y2] [(* width (/ 2 3)) (/ height 2)]
        p (take 100 (iterate (partial points-generation
                                      (fn [] {:centroid [(/ width 2) (/ height 2)]
                                              :n-points 50})
                                      (fn
                                        ([] (+ (rand-int 50) 4))
                                        ([old-length] (* old-length 1.04))
                                        #_([old-length] (+ old-length (Math/sqrt old-length)))))
                             []))]

    (doseq [point-group (drop 1 p)]

      (q/begin-shape)
      (doseq [point (:points point-group)]
        (apply q/curve-vertex ((juxt :x :y) point)))

      #_(let [pg (:points point-group)
            one (first pg)
            two (second pg)
            three (first (reverse pg))
            four (second (reverse pg))
            ]
        (apply q/curve-vertex ((juxt :x :y) one))
        ()
        )
      (q/end-shape :close)


      )))

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
