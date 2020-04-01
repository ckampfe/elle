(ns ckampfe.over3
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

(defn rat-to-xy [[xrat yrat] column row offset]
  [(+ (* xrat offset) (* column offset))
   (+ (* yrat offset) (* row offset))])

(defn draw []
  #_(q/curve-tightness -4.0)
  (let [[columns rows] (tiles width height 60)]
    (let [rats [[0 0]
                [1/2 1]
                [1 0]
                [0 0]]]

      (doseq [column (range columns)
              row (range (- rows 2))]
        (let [coords (map (fn [rat] (rat-to-xy rat column row 60)) rats)]

          (q/begin-shape)
          (doseq [coord coords]
            (apply q/curve-vertex coord))
          (q/end-shape :close))))))

(try (q/defsketch example
       :title "pendulums"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [width height]
       ;; :renderer :svg
       ;; :output-file "over3.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
