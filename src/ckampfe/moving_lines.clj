(ns ckampfe.moving-lines
  (:require [quil.core :as q]))

(defn pendulum [min-dim max-dim tick-size]
  (let [state (atom {:position min-dim
                     :direction :positive})]
    (fn []
      (case (:direction @state)
        :positive (if (<= (+ (:position @state) (Math/sqrt (- max-dim (:position @state))))
                          max-dim)
                    (swap! state update :position (partial + tick-size)#_(partial + (Math/sqrt (- max-dim (:position @state)))))
                    (do
                      (swap! state update :position (fn [old] (- old (Math/sqrt (- old min-dim)))))
                      (swap! state assoc :direction :negative)))

        :negative (if (<= min-dim
                          (- (:position @state) (Math/sqrt (- (:position @state) min-dim))))
                    (swap! state update :position (fn [old] (- old  (Math/sqrt (- old min-dim)) #_tick-size  )))
                    (do
                      (swap! state update :position (partial + tick-size))
                      (swap! state assoc :direction :positive)))))))

(defn setup []
  (q/background 255)
  (q/no-stroke)
  (q/no-loop))

(defn draw []
  (q/fill 100 100 100)

  (let [x1 (pendulum 40 520 14)
        x2 (pendulum 680 1160 6)
        y1 (pendulum 50 350 20)
        y2 (pendulum 400 700 7)
        ;; pendulums [x1 x2 y1 y2]

       ;;  x1s (map (fn [_] (:position (x1)))
       ;;           (range 1700))
       ;;  x2s (map (fn [_] (:position (x2)))
       ;;           (range 1700))
       ;;  y1s (map (fn [_] (:position (y1)))
       ;;           (range 1700))
       ;;  y2s (map (fn [_] (:position (y2)))
       ;;           (range 1700))

       ;;  x1y1 (map (fn [x y]
       ;;              [x y])
       ;;            x1s y1s)
       ;;  x2y1 (map (fn [x y]
       ;;              [x y])
       ;;            x2s y1s)
       ;;  x1y2 (map (fn [x y]
       ;;              [x y])
       ;;            x1s y2s)
       ;;  x2y2 (map (fn [x y]
       ;;              [x y])
       ;;            x2s y2s)
        ]

    #_(doseq [pair-list [x1y1 x2y1 x1y2 x2y2]
            [[x1 y1] [x2 y2]] (partition 2 1 pair-list)
            ]
      (q/line x1 y1 x2 y1))



    (dotimes [n 3300]
        (let [x1t (:position (x1))
              x2t (:position (x2))
              y1t (:position (y1))
              y2t (:position (y2))]
          (q/ellipse x1t y1t 4 4)
          (q/ellipse x1t y2t 4 4)
          (q/ellipse x2t y1t 4 4)
          (q/ellipse x2t y2t 4 4)))))

(try (q/defsketch example
       :title "pendulums"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       :renderer :svg
       :output-file "moving-lines-1.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
