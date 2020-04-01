(ns ckampfe.worley4
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

(defn intersection [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (try
    #_(and (not= [x1 y1] [x2 y2])
           (not= [x1 y1] [x3 y3])
           (not= [x1 y1] [x4 y4])

           (not= [x2 y2] [x3 y3])
           (not= [x2 y2] [x4 y4]))
    (let [num-x (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4))
                   (* (- x1 x2) (- (* x3 y4) (* y3 x4))))
          denom-x (- (* (- x1 x2) (- y3 y4))
                     (* (- y1 y2) (- x3 x4)))

          x (/ num-x
               denom-x)

          num-y (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4))
                   (* (- y1 y2) (- (* x3 y4) (* y3 x4))))

          denom-y (- (* (- x1 x2) (- y3 y4))
                     (* (- y1 y2) (- x3 x4)))

          y (/ num-y denom-y)

          sorted-x-1 (sort [x1 x2])
          min-x-1 (first sorted-x-1)
          max-x-1 (last sorted-x-1)

          sorted-y-1 (sort [y1 y2])
          min-y-1 (first sorted-y-1)
          max-y-1 (last sorted-y-1)

          sorted-x-2 (sort [x3 x4])
          min-x-2 (first sorted-x-2)
          max-x-2 (last sorted-x-2)

          sorted-y-2 (sort [y3 y4])
          min-y-2 (first sorted-y-2)
          max-y-2 (last sorted-y-2)]

      (when (and (>= x min-x-1)
                 (>= x min-x-2)

                 (<= x max-x-1)
                 (<= x max-x-2)

                 (>= y min-y-1)
                 (>= y min-y-2)

                 (<= y max-y-1)
                 (<= y max-y-2))

        [x y]))
    (catch ArithmeticException e nil)))

#_(intersection [-20 -20 20 20]
                [20 -20 -20 20])

#_(intersection [-20 -20 -20 20]
                [20 -20 20 20])

(defn aabb [points]
  (let [xs (sort (map :x points))
        ys (sort (map :y points))
        min-x (first xs)
        max-x (last xs)
        min-y (first ys)
        max-y (last ys)]

    [min-x min-y max-x max-y]))

(defn within? [[min-x min-y max-x max-y] [x y]]
  (and (>= x min-x)
       (>= y min-y)
       (<= x max-x)
       (<= y max-y)))

(defn draw []
  (q/no-fill)
  (let [[x1 y1] [(/ width 3) (/ height 2)]
        [x2 y2] [(* width (/ 2 3)) (/ height 2)]
        p1 (take 2 (iterate (partial points-generation
                                     (fn [] {:centroid [(- (/ width 2) 20) (- (/ height 2) 20)]
                                             :n-points 30})
                                     (fn
                                       ([] (+ 50 #_(rand-int 50) 4))
                                       #_([old-length] (* old-length 1.021))
                                       ([old-length] (* old-length 3))
                                       #_([old-length] (->> old-length
                                                            Math/sqrt
                                                            (* 6)
                                                            (/ 300)
                                                            (+ old-length)))))
                            []))
        p2 (take 2 (iterate (partial points-generation
                                     (fn [] {:centroid [(+ (/ width 2) 20) (+ (/ height 2) 20)]
                                             :n-points 30})
                                     (fn
                                       ([] (+ 50 #_(rand-int 50) 4))
                                       #_([old-length] (* old-length 1.021))
                                       ([old-length] (* old-length 3))
                                       #_([old-length] (->> old-length
                                                            Math/sqrt
                                                            (* 6)
                                                            (/ 300)
                                                            (+ old-length)))))
                            []))
        lines1 (partition 2 1 (:points (first (drop 1 p1))) #_(:points (first (drop 1 p1))))
        lines2 (partition 2 1 (:points (first (drop 1 p2))) #_(:points (first (drop 1 p2))))
        intersections (filter #(not (nil? %))
                              (for [line1 lines1
                                    line2 lines2]

                                (let [l1 (mapv (fn [p] ((juxt :x :y) p)) line1)
                                      l2 (mapv (fn [p] ((juxt :x :y) p)) line2)]

                                  (intersection l1
                                                l2))))

        p1-aabb (aabb (:points (first (drop 1 p1))))
        p2-aabb (aabb (:points (first (drop 1 p2))))
        non-overlapped-p1 (filter (fn [point]
                                    (not (within? p2-aabb ((juxt :x :y) point))))
                                  (:points (first (drop 1 p1))))
        non-overlapped-p2 (filter (fn [point]
                                    (not (within? p1-aabb ((juxt :x :y) point))))
                                  (:points (first (drop 1 p2))))]

    #_(println "p1s" non-overlapped-p1)
    #_(println "p2s" non-overlapped-p2)

    #_(doseq [[x1 y1 x2 y2] [p1-aabb p2-aabb]]
        (q/begin-shape :lines)
        (q/vertex x1 y1)
        (q/vertex x2 y1)
        (q/vertex x2 y2)
        (q/vertex x1 y2)
        (q/vertex x1 y1)
        (q/end-shape :close))

    (doseq [i intersections]
      (apply q/ellipse (conj i 8 8)))

    (doseq [pg [non-overlapped-p1 non-overlapped-p2]
            point pg]

      (q/begin-shape)
      (doseq [point pg]
        (apply q/curve-vertex ((juxt :x :y) point)))

      #_(let [pg (:points point-group)
              one (first pg)
              two (second pg)
              three (first (reverse pg))
              four (second (reverse pg))]
          (apply q/curve-vertex ((juxt :x :y) one))
          ())
      (q/end-shape :close))

    ;; (doseq [p [p1 p2]
    #_(doseq [p [non-overlapped-p1 non-overlapped-p2]
              point-group (drop 1 p)]

        (q/begin-shape)
        (doseq [point (conj (:points point-group)
                            (first (:points point-group)))]
          (apply q/curve-vertex ((juxt :x :y) point)))

        #_(let [pg (:points point-group)
                one (first pg)
                two (second pg)
                three (first (reverse pg))
                four (second (reverse pg))]
            (apply q/curve-vertex ((juxt :x :y) one))
            ())
        (q/end-shape :close))))

(try (q/defsketch example
       :title "worley"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :svg
       ;; :output-file "topocircle1.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
