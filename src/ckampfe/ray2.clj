(ns ckampfe.ray2
  (:require [quil.core :as q]
            [clojure.core.matrix :as m]
            [mikera.vectorz.core :as v]))

(m/set-current-implementation :vectorz)

(def width 1200)
(def height 800)

(defn setup []
  (q/background 255)
  (q/no-fill)
  (q/no-loop))

(defn rotate-wrt-point [theta [cx cy] [px py]]
  [(+ (- (* (Math/cos theta)
            (- px cx))
         (* (Math/sin theta)
            (- py cy)))
      cx)

   (+ (+ (* (Math/sin theta)
            (- px cx))
         (* (Math/cos theta)
            (- py cy)))
      cy)])

(defn surface-normals-at-intersection
  "returns reflected vector starting at the intersection point"
  [circle-centroid r intersection-point]
  (let [radius-v (m/sub intersection-point circle-centroid)
        xxx (println "rv" radius-v)
        intersection-normal (m/add radius-v intersection-point)]
    intersection-normal))

(defn reflect-ray-off-circle
  "returns reflected vector starting at the intersection point:
  R=2(N^⋅L^)N^−L^
  https://www.fabrizioduroni.it/2017/08/25/how-to-calculate-reflection-vector.html
  "
  [circle-centroid r start-point intersection-point]
  (let [ray-v (m/normalise (m/sub start-point intersection-point))
        radius-v (m/normalise (m/sub intersection-point circle-centroid))
        intersection-normal (m/normalise (m/mul radius-v intersection-point))
        reflection-v (m/sub (m/mul 2 (m/dot intersection-normal ray-v) intersection-normal) ray-v)]

    (println "angle between ray-v and normal" (v/angle (v/vec ray-v) (v/vec intersection-normal)))
    (println "angle between reflection-v and normal" (v/angle (v/vec reflection-v) (v/vec intersection-normal)))

    reflection-v))

(defn generate-rays
  "https://stackoverflow.com/questions/2259476/rotating-a-point-about-another-point-2d"
  [theta [cx cy] start-point]
  (iterate (fn [[px py]]
             (rotate-wrt-point theta [cx cy] [px py]))
           start-point))

(defn intersections [[cx cy] r [x1 y1] [x2 y2]]
  (let [to-origin (m/sub [0 0] [cx cy])
        [x1 y1] (m/add [x1 y1] to-origin)
        [x2 y2] (m/add [x2 y2] to-origin)
        r2 (* r r)
        dx (- x2 x1)
        dy (- y2 y1)

        dr (Math/sqrt (+ (* dx dx)
                         (* dy dy)))
        dr2 (* dr dr)
        d (- (* x1 y2)
             (* x2 y1))
        d2 (* d d)
        discriminant (- (* r2 dr2) d2)]

    (cond
      (= discriminant 0)
      (let [x (/ (+ (* d dy) (* (if (< dy 0) -1 1)
                                dx
                                (Math/sqrt (- (* r2 dr2) d2))))
                 dr2)

            y (/ (+ (* (- d) dx) (* (Math/abs dy) (Math/sqrt (- (* r2 dr2)
                                                                d2))))
                 dr2)]

        [(m/sub [x y] to-origin)])

      (> discriminant 0) (let [pairs (map (fn [op]
                                            (let
                                             [x (/ (op (* d dy) (* (if (< dy 0) -1 1)
                                                                   dx
                                                                   (Math/sqrt (- (* r2 dr2) d2))))
                                                   dr2)

                                              y (/ (op (* (- d) dx) (* (Math/abs dy) (Math/sqrt (- (* r2 dr2)
                                                                                                   d2))))
                                                   dr2)]
                                              (m/sub [x y] to-origin)))
                                          [+ -])]

                           (println pairs)

                           pairs)

      :else [])))

(defn dist [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)

        dr (Math/sqrt (+ (* dx dx)
                         (* dy dy)))]
    dr))

(defn draw []
  (println "----------------")
  ;; circle
  #_(q/with-fill [50 50 50]
      (q/ellipse 200 (/ height 2) 100 100))

  ;; circle centroid
  #_(q/with-fill [200 20 20]
      (q/ellipse 200 (/ height 2) 15 15))

  #_(q/line 100 615 1100 615)

  (let [circle-radius 50

        circle-centroid [(/ width 2) (/ height 2)]

        ray-origin [1100 400]

        rays (take 50 (generate-rays (Math/toRadians 0.5)
                                     ray-origin
                                     [100 615]))

        ray2-origin [100 400]

        rays2 (take 50 (generate-rays (Math/toRadians 0.5)
                                      ray2-origin
                                      [1100 177]))]
    (doseq [[rayx rayy] rays]
      (let [is (intersections
                circle-centroid
                circle-radius
                ray-origin
                [rayx rayy])]
        (cond
          (= 2 (count is))
          (doseq [[ix iy] (take 1 (sort-by (fn [p]
                                             (dist p ray-origin))
                                           is))]

            (let [[iinx iiny] (surface-normals-at-intersection circle-centroid
                                                               circle-radius
                                                               [ix iy])
                  [inx iny] (m/add [ix iy] [iinx iiny])
                  xxx (println ">>>>>>>")
                  xxx (println "ix iy" [ix iy])
                  xxx (println "iinx iiny" [iinx iiny])
                  xxx (println "inx iny" [inx iny])
                  [rpx rpy] (reflect-ray-off-circle circle-centroid
                                                    circle-radius
                                                    ray-origin
                                                    [ix iy])
                  [rrpx rrpy] (m/add [ix iy] (m/mul [rpx rpy] 700))]

              ;; the ray
              (q/with-stroke 50
                (q/line (first ray-origin) (second ray-origin) ix iy))

              ;; the radius normal
              #_(q/with-stroke [200 200 80]
                  (q/line 200 (/ height 2) ix iy))

              ;; the reflection ray??????
              (q/with-stroke [20 200 50]
                (q/line ix iy rrpx rrpy))

              ;; the surface-normal
              #_(q/with-stroke [20 50 200]
                  (q/line ix iy iinx iiny)))

            ;; intersection point
            #_(q/with-fill [200 20 20]
                (q/ellipse ix iy 10 10)))

          (= 1 (count is))
          (doseq [[ix iy] is]

            (println "ix iy" [ix iy])
            (q/with-stroke 50
              (q/line 1000 400 rayx rayy))

            (q/with-fill [200 20 20]
              (q/ellipse ix iy 10 10)))

          :else (do
                  (q/with-stroke 50
                    (q/line (first ray-origin) (second ray-origin) rayx rayy))))))

    (doseq [[rayx rayy] rays2]
      (let [is (intersections
                [(/ width 2) (/ height 2)]
                circle-radius
                ray2-origin
                [rayx rayy])]
        (cond
          (= 2 (count is))
          (doseq [[ix iy] (take 1 (sort-by (fn [p]
                                             (dist p ray2-origin))
                                           is))]

            (let [[iinx iiny] (surface-normals-at-intersection circle-centroid
                                                               circle-radius
                                                               [ix iy])
                  [inx iny] (m/add [ix iy] [iinx iiny])
                  xxx (println ">>>>>>>")
                  xxx (println "ix iy" [ix iy])
                  xxx (println "iinx iiny" [iinx iiny])
                  xxx (println "inx iny" [inx iny])
                  [rpx rpy] (reflect-ray-off-circle circle-centroid
                                                    circle-radius
                                                    ray2-origin
                                                    [ix iy])
                  [rrpx rrpy] (m/add [ix iy] (m/mul [rpx rpy] 700))]

              ;; the ray
              (q/with-stroke 50
                (q/line (first ray2-origin) (second ray2-origin) ix iy))

              ;; the radius normal
              #_(q/with-stroke [200 200 80]
                  (q/line 200 (/ height 2) ix iy))

              ;; the reflection ray??????
              (q/with-stroke [20 200 50]
                (q/line ix iy rrpx rrpy))

              ;; the surface-normal
              #_(q/with-stroke [20 50 200]
                  (q/line ix iy iinx iiny)))

            ;; intersection point
            #_(q/with-fill [200 20 20]
                (q/ellipse ix iy 10 10)))

          (= 1 (count is))
          (doseq [[ix iy] is]

            (println "ix iy" [ix iy])
            (q/with-stroke 50
              (q/line 1000 400 rayx rayy))

            (q/with-fill [200 20 20]
              (q/ellipse ix iy 10 10)))

          :else (do
                  (q/with-stroke 50
                    (q/line (first ray2-origin) (second ray2-origin) rayx rayy))))))))

(try (q/defsketch example
       :title "hull"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [width height]
       ;; :renderer :svg
       ;; :output-file "ray2.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
