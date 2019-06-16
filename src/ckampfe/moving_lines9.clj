(ns ckampfe.moving-lines9
  (:require [quil.core :as q]))

(defn pendulum [min-dim max-dim tick-size]
  (let [state (atom {:position min-dim
                     :direction :positive})]
    (fn []
      (case (:direction @state)
        :positive (if (<= (+ (:position @state) (Math/sqrt (- max-dim (:position @state))))
                          max-dim)

                    #_(swap! state update :position (fn [old] (+ old (* (Math/abs (Math/cos old))
                                                                      tick-size))))
                    (swap! state update :position (fn [old] (+ old tick-size)))

                    (do
                      (swap! state update :position (fn [old] (- old tick-size)))

                      (swap! state assoc :direction :negative)))

        :negative (if (<= min-dim
                          (- (:position @state) (Math/sqrt (- (:position @state) min-dim))))

                    #_(swap! state update :position (fn [old] (- old (* (Math/abs (Math/cos old))
                                                                      tick-size))))
                    (swap! state update :position (fn [old] (- old tick-size)))

                    (do
                      #_(swap! state update :position (fn [old] (+ old (Math/sqrt old))))
                      (swap! state update :position (fn [old] (+ old tick-size)))

                      (swap! state assoc :direction :positive)))))))

(defn setup []
  (q/background 255)
  #_(q/no-stroke)
  #_(q/frame-rate 1)
  (q/no-fill)
  (q/no-loop))

#_(def runs (atom 1))

(defn draw []
  #_(q/fill 100 100 100)

  (let [xwidth 700
        ywidth 700
        buffer-width 40

        ;; x1 (pendulum 120 xwidth 19)
        ;; y1 (pendulum 120 ywidth 15)
        x1 (pendulum 120 xwidth 3)
        y1 (pendulum 120 ywidth 4)

        points (map
                (fn [i]
                  [(:position (x1)) (:position (y1))])

                (range 5865)
                )
        ]


    #_(println (take 5 points))

    #_(doseq [pgroup (take 1000 (partition 4 1 points))]
        #_(println pgroup)
        #_(println (flatten pgroup))
        (apply q/curve (flatten pgroup)))

    (q/begin-shape)
    (doseq [point points]
      (q/curve-tightness 1.1)
      (apply q/curve-vertex point))
    (q/end-shape :close)

    #_(dotimes [n 3380]
        (let [x1t (:position (x1))
            ;; x2t (:position (x2))
              y1t (:position (y1))
            ;; ky2t (:position (y2))
              ]
          (q/ellipse x1t y1t 5 5)
        ;; (q/ellipse x1t y2t 4 4)
          #_(q/ellipse x2t y1t 4 4)
          #_(q/ellipse x2t y2t 4 4)))))

(try (q/defsketch example
       :title "pendulums"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :svg
       ;; :output-file "moving-lines-9.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
