(ns ckampfe.voronoi
  (:require [quil.core :as q]
            [clojure.core.matrix :as m]
            [clojure.core.reducers :as r]
            [clojure.set :as set]))

(def width 1200)

(def height 800)

(defn gen-points [n width height]
  (map (fn [_]
         [(rand-int width)
          (rand-int height)])
       (range n)))

;; {[x y] -> [[dist [x y]] ...] ...}
#_(defn dist-map [points]
    (let [xs (range width)
          ys (range height)
          kv (time (pmap
                    (fn [[x1 y1]]
                      [[x1 y1]
                       (take 2 (sort-by first
                                        (for [[x2 y2] points]
                                          [(q/dist x1 y1 x2 y2) [x2 y2]])))])
                    (for [x xs
                          y ys]
                      [x y])))]
      (time (into {} kv))))

(defn dist-map [points]
  (let [kv (pmap
            (fn [[x1 y1]]
              [[x1 y1]
               (->> (for [[x2 y2] points] [(q/dist x1 y1 x2 y2) [x2 y2]])
                    (sort-by first)
                    (filter (fn [[d [x2 y2]]]
                              (not= [x1 y1] [x2 y2])))
                    (take 2))])
            points)]
    (into {} kv)))

(defn circumcenter-xy
  "https://en.wikipedia.org/wiki/Circumscribed_circle#Cartesian_coordinates_2"
  [[ax ay] [bx by] [cx cy]]
  (let [d (* 2 (+ (* ax (- by cy))
                  (* bx (- cy ay))
                  (* cx (- ay by))))
        ux (* (/ 1 d) (+ (* (+ (Math/pow ax 2) (Math/pow ay 2))
                            (- by cy))
                         (* (+ (Math/pow bx 2) (Math/pow by 2))
                            (- cy ay))
                         (* (+ (Math/pow cx 2) (Math/pow cy 2))
                            (- ay by))))
        uy (* (/ 1 d) (+ (* (+ (Math/pow ax 2) (Math/pow ay 2))
                            (- cx bx))
                         (* (+ (Math/pow bx 2) (Math/pow by 2))
                            (- ax cx))
                         (* (+ (Math/pow cx 2) (Math/pow cy 2))
                            (- bx ax))))]

    [ux uy]))

(defn triangles [distmap]
  (reduce (fn [acc [p1 [[_ p2]
                        [_ p3]]]]
            (conj acc (set (vector p1 p2 p3))))
          #{}
          distmap))

(defn setup []
  (q/background 255)
  (q/no-loop))

(defn draw []
  (let [points (gen-points 9 1000 600)
        ;; [cx cy] (apply circumcenter-xy points)
        distmap (dist-map points)
        ts (triangles distmap)
        t-circumcenters (map (fn [t] (apply circumcenter-xy t)) ts)
        tc-distmap (dist-map t-circumcenters)
        tc-triangles (triangles tc-distmap)]

    (println ts)

    (q/with-fill [255 255 255]
      (doseq [[x y] points]
        (q/ellipse x y 5 5)))

    (q/with-fill [255 0 0]
      (doseq [t tc-triangles]
        (let [[p1 p2 p3] (seq t)]

          (q/line p1 p2)
          (q/line p1 p3)
          (q/line p2 p3))))

    #_(q/with-fill [255 0 0]
        (doseq [t ts]
          (let [[cx cy] (apply circumcenter-xy t)]
            (q/ellipse cx cy 5 5))))))

(try (q/defsketch example
       :title "voronoi"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :svg
       ;; :output-file "rot3.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
