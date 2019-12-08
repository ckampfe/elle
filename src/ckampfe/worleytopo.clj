(ns ckampfe.worleytopo
  (:require [quil.core :as q]
            [clojure.core.matrix :as m]
            [clojure.core.reducers :as r]))

(def width 1200)

(def height 800)

(defn find-highpoints [pixels]
  (let [pixels-with-channel-sums (map-indexed (fn [i pixel]
                                                (let [x (mod i width)
                                                      y (quot i width)
                                                      channel-sum (reduce + ((juxt q/red q/green q/blue)
                                                                             pixel))]
                                                  {:x x
                                                   :y y
                                                   :pixel pixel
                                                   :channel-sum channel-sum}))

                                              pixels)
        max-pixel (apply max-key :channel-sum pixels-with-channel-sums)
        min-pixel (apply min-key :channel-sum pixels-with-channel-sums)
        ;; max-minus-five #_(* 0.90 (:channel-sum min-pixel))
        min-pixels (filter (fn [{:keys [channel-sum]}]
                             ;; (<= channel-sum max-minus-five))
                             (<= channel-sum (:channel-sum min-pixel)))
                           pixels-with-channel-sums)]
    (println min-pixel)
    (println ((juxt q/red q/green q/blue)
              (:pixel min-pixel)))
    (println (count min-pixels))
    {:min-pixels min-pixels
     :min-pixel min-pixel
     :max-pixel max-pixel}))

(defn setup []
  (q/no-loop)
  (q/background 255)
  (q/set-state! :image (q/load-image "worley.png")))

(defn draw []
  #_(let [pixels (q/pixel)
          highest-points (find-highest-points pixels)])
  (let [image (q/state :image)]
    (loop []
      (if (q/loaded? image)
        (do (q/image image 0 0)
            (let [pixels (q/pixels image)
                  {highpoints :min-pixels
                   min-pixel :min-pixel
                   max-pixel :max-pixel} (find-highpoints pixels)]
              (doseq [{:keys [x y]} highpoints]
                (q/with-stroke [40 200 40]
                  (q/ellipse x y 10 10)))

              (doseq [{:keys [x y]} (take 1 (drop 5 highpoints))]
                (q/with-stroke [40 200 40]
                  (q/no-fill)
                  (q/ellipse x y 40 40)))

              (println "lerp" ((juxt q/red q/green q/blue)
                               (q/lerp-color (apply q/color ((juxt q/red q/green q/blue)
                                                             (:pixel min-pixel)))
                                             (apply q/color ((juxt q/red q/green q/blue)
                                                             (:pixel max-pixel)))
                                             0.05)))))

        (recur)))
    #_(when (q/loaded? image)
        (println (q/loaded? image)))))

(try (q/defsketch example
       :title "worleytopo"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :java2d
       ;; :output-file "worley.png"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
