(ns ckampfe.waves2
  (:require [quil.core :as q]))

(def sins (concat
           (reverse (range 2 50 1))
           (range 1 51 1)))

sins

(def base-distance 100)

(defn setup []
  (q/background 255)
  (q/no-fill)
  (q/no-loop))

(defn draw []
  (let [xs (reduce (fn [acc [x sin-modifier]]
                     (conj acc (+ (peek acc) (int (Math/ceil (/ base-distance sin-modifier))))))
                   [10]
                   (map (fn [x sin-modifier]
                          [x sin-modifier])
                        (iterate (fn [x] (+ 10 x)) 0)
                        sins))

        ys (reduce (fn [acc [y sin-modifier]]
                     (conj acc (+ (peek acc) (int (Math/ceil (/ base-distance sin-modifier))))))
                   [10]
                   (map (fn [y sin-modifier]
                          [y sin-modifier])
                        (iterate (fn [y] (+ 10 y)) 0)
                        sins))

        xmax (peek xs)
        ymax (peek ys)]

    (doseq [x xs]
      (q/line x 10 x ymax))

    (doseq [y ys]
      (q/line 10 y xmax y))))

(try (q/defsketch example
       :title "waves2"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 860]
       ;; :renderer :svg
       ;; :output-file "waves2.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
