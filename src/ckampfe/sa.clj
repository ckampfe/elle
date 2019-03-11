(ns ckampfe.elle.sa
  (:require [quil.core :as q]
            #_[clojure.core.matrix :as m]))

(set! *warn-on-reflection* true)

#_(m/set-current-implementation :vectorz)

(def width 1280)
(def height 800)

(def cmyk [:c :m :y :k])

(defn q-energy
  "must be less than qmin, must be greater than qmax"
  [qmin qmax]
  (fn [mat]

    (let [distances (:distances mat)

          with-indexes (zipmap (range (count (:data mat))) (:data mat))

          c-points (filter #(= (second %) :c) with-indexes)
          m-points (filter #(= (second %) :m) with-indexes)
          y-points (filter #(= (second %) :y) with-indexes)
          k-points (filter #(= (second %) :k) with-indexes)

          c-pairings (for [[i1 p1] c-points
                           [i2 p2] c-points
                           :when (not= i1 i2)]
                       (set [[(quot i1 (:height mat))
                              (mod i1 (:height mat))]
                             [(quot i2 (:height mat))
                              (mod i2 (:height mat))]]))
          m-pairings (for [[i1 p1] m-points
                           [i2 p2] m-points
                           :when (not= i1 i2)]
                       (set [[(quot i1 (:height mat))
                              (mod i1 (:height mat))]
                             [(quot i2 (:height mat))
                              (mod i2 (:height mat))]]))

          y-pairings (for [[i1 p1] y-points
                           [i2 p2] y-points
                           :when (not= i1 i2)]
                       (set [[(quot i1 (:height mat))
                              (mod i1 (:height mat))]
                             [(quot i2 (:height mat))
                              (mod i2 (:height mat))]]))

          k-pairings (for [[i1 p1] k-points
                           [i2 p2] k-points
                           :when (not= i1 i2)]
                       (set [[(quot i1 (:height mat))
                              (mod i1 (:height mat))]
                             [(quot i2 (:height mat))
                              (mod i2 (:height mat))]]))

          c-distances (vals (select-keys distances c-pairings))
          m-distances (vals (select-keys distances m-pairings))
          y-distances (vals (select-keys distances y-pairings))
          k-distances (vals (select-keys distances k-pairings))

          d-fn (fn [distance]
                 (or (< distance qmin) (> distance qmax)))

          height (:height mat)

          score-mapping (fn [distance]
                          (if (and (< distance qmax)
                                   (> distance qmin))
                            3
                            0
                            )
                          #_(cond (> distance qmax) (Math/floor distance)
                                (< distance qmin) 30
                                :else 0))

          ;; c-score (count (filter d-fn c-distances))
          ;; m-score (count (filter d-fn m-distances))
          ;; y-score (count (filter d-fn y-distances))
          ;; k-score (count (filter d-fn k-distances))

          c-score (future (reduce + (map score-mapping c-distances)))
          m-score (future (reduce + (map score-mapping m-distances)))
          y-score (future (reduce + (map score-mapping y-distances)))
          k-score (future (reduce + (map score-mapping k-distances)))

          energy (reduce + [@c-score @m-score @y-score @k-score])]

      energy
      ;; in case energy is awful
      #_(if (= energy 0)
          0.000000000000001
          energy))))

(defn random-point []
  (first (shuffle cmyk)))

(defn positions [x y]
  (for [yy (range y)
        xx (range x)]
    [yy xx]))

(defn distance [x1 y1 x2 y2]
  (Math/sqrt ^double (+ (Math/pow ^double (- x1 x2) 2)
                        (Math/pow ^double (- y1 y2) 2))))

(defn compute-distances [positions]
  (reduce (fn [acc pair]
            (if (get acc pair)
              acc
              (let [[x1 y1] (first pair)
                    [x2 y2] (second pair)]
                (assoc acc
                       pair
                       (distance x1 y1 x2 y2)))))
          {}
          (for [p1 positions p2 positions
                :when (not= p1 p2)]
            #{p1 p2})))

(defn neighbors-for-point [x y width height]
  (filter (fn [[new-y new-x]]
            (and (not= [new-y new-x] [y x])
                 (< new-x width)
                 (< new-y height)
                 (>= new-x 0)
                 (>= new-y 0)))

          (for [y-op [inc dec nil]
                x-op [inc dec nil]]
            (let [new-x (if (nil? x-op)
                          x
                          (x-op x))
                  new-y (if (nil? y-op)
                          y
                          (y-op y))]
              [new-y new-x]))))

(defn compute-neighbors [width height positions]
  (reduce (fn [acc [y x]]
            (assoc acc
                   [y x]
                   (neighbors-for-point x y width height)))
          {}
          positions))

(defn new-matrix [width height]
  (let [positions (positions width height)]
    {:data (shuffle (take (* width height) (cycle [:c :m :y :k])))
     :width width
     :height height
     :neighbors (compute-neighbors width height positions)
     :distances (compute-distances positions)}))

(defn temperature
  "temperature falls as k-over-kmax increases"
  [k-over-kmax]
  (* 1 k-over-kmax))

(defn annealing-probability
  "temp is rational value between 1 and 0.

   When T tends to zero, the probability P(e,e',T) must tend to zero if e' > e and to a positive value otherwise.

  energy-s and energy-new-s are both floats, the consine sim of target and state (or new prospective state)"
  [energy-s energy-new-s temperature]
  (let [;; normalized (- energy-s energy-new-s)
]
    (if #_(< energy-s energy-new-s)
      (< energy-new-s energy-s)
      1.1
      (- 1 (Math/pow (/ (- energy-new-s energy-s)
                        energy-s)
                     temperature))
      #_(- 1 (Math/pow (/ (- energy-s energy-new-s)
                        energy-s)
                     temperature)))))

#_(defn random-new-neighbor [s temperature energy-fn]
    (let [s-energy (energy-fn s)
          massaged (set (flatten s))
          shuffled (shuffle massaged)
          s-new (filter (fn [edge]
                          (> (count edge) 1))
                        (map vec (partition-all 2 1 shuffled)))

          s-new-energy (energy-fn s-new)]
      s-new))

(defn random-new-neighbor [s temperature energy-fn]
  (let [points (:data s)
        grouped (group-by identity points)
        ;; partitioned (partition (:height s) points)
        with-index (zipmap points (range))
        new-points (reduce (fn [acc-points [point-color i]]
                             (if (>= (rand) 0.1)
                               (let [point-yx [(quot i (:height s))
                                               (mod i (:height s))]
                                     point-color (get-in acc-points point-yx)
                                     neighbors (:neighbors s)
                                     neighbors-of-point (get neighbors point-yx)
                                     neighbor-yx (first (shuffle neighbors-of-point))
                                     neighbor-color (get-in acc-points neighbor-yx)
                                     new-points (-> acc-points
                                                    (assoc-in point-yx neighbor-color)
                                                    (assoc-in neighbor-yx point-color))

                                     gin (set (map (fn [[color color-seq]] [color (count color-seq)])
                                                   (group-by identity (flatten acc-points))))
                                     gout (set (map (fn [[color color-seq]] [color (count color-seq)])
                                                    (group-by identity (flatten new-points))))]

                                 #_(println "gin" gin)
                                 #_(println "gout" gout)
                                 #_(assert (= gin gout)
                                           (str "i: " i
                                                ",\n computed point-yx: " point-yx
                                                ",\n neighbor-yx: " neighbor-yx
                                                ",\n old point color: " point-color
                                                ",\n old neighbor-color: " neighbor-color
                                                ",\n new point color: " (get-in new-points point-yx)
                                                ",\n new neighbor color: " (get-in new-points neighbor-yx)))

                                 new-points)
                               acc-points))

                           (mapv vec (partition (:height s) points))
                           with-index)]

    #_(assoc s :data (shuffle points))
    (assoc s :data (flatten new-points))))

(defn simulated-annealing
  "When T tends to zero, the probability P(e,e',T) must tend to zero if e' > e and to a positive value otherwise."
  [initial-s kmax]
  (let [energy-fn (q-energy 1 3)]
    (reduce (fn [[acc s] k]
              (let [temperature (temperature (- 1 (/ k kmax)))
                    s-new (random-new-neighbor s temperature energy-fn)
                    this-rand (rand)
                    energy-s (energy-fn s)
                    energy-s-new (energy-fn s-new)
                    ap (annealing-probability energy-s
                                              energy-s-new
                                              temperature)]


                (if (and (> ap this-rand)
                         #_(not= s (reverse s-new))
                         #_(not= s (rseq (vec s-new)))
                         #_(not= s s-new)
                         (not= energy-s
                               energy-s-new))
                  (do
                    (println "energy-s-new:" energy-s-new)
                    (println "ap:" ap)
                    (println "this rand:" this-rand)
                    [(conj acc [s-new energy-s-new])
                    s-new])
                  [acc s])))
            [[] initial-s]
            (range kmax))))

(defn fill-for [color]
  (case color
    :c [10 120 200 190]
    :m [200 10 100 190]
    :y [250 200 10 190]
    :k [0 0 0 45]))

(defn draw []
  (q/no-stroke)

  (let [[sims initial-sim] (simulated-annealing (new-matrix 15 15) 10000)]
    (doall (map-indexed (fn [col-number part]
                          (doall (map-indexed (fn [row-number [s energy]]
                                                (let [adjx (+ 40 (* row-number 230))
                                                      adjy (+ 20 (* col-number 220))]
                                                  (q/with-fill [200 200 200]
                                                    (q/text (.toString energy)
                                                            (+ 10  adjx)
                                                            (+ 7 adjy)))
                                                  (doall (map-indexed (fn [y points-row]

                                                                        (doall (map-indexed (fn [x color]
                                                                                              (q/with-fill (fill-for color)
                                                                                                (q/rect (+ (* 10 (+ 1 x)) adjx)
                                                                                                        (+ (* 10 (+ 1 y)) adjy)
                                                                                                        10
                                                                                                        10)))
                                                                                            points-row)))
                                                                      (partition (:height s)
                                                                                 (:data s))))))
                                              part)))
                        #_[(take 1 (reverse sims))]
                        (->> sims
                             reverse
                             #_(take 15)
                             (take-nth 300)
                             reverse
                             (partition-all 4)
                             (take 3))
                        #_(partition 5 (reverse (take 15 (reverse sims))))))))

(defn setup []
  (q/background 255)
  (q/no-loop))

(q/defsketch example
  :title "crystal"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :size [1200 800]
  ;; :renderer :svg
  ;; :output-file "entropylines.svg"
  :features [:no-bind-output])
