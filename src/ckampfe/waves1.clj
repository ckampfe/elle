(ns ckampfe.curve12
  (:require [quil.core :as q]))

(def sins (concat
           (reverse (range 2 10 1))
           (range 1 11 1)
           ))

(def base-distance 30)

(defn setup []
  (q/background 255)
  (q/no-fill)
  (q/no-loop))

(defn draw []
  (doseq [x (reduce (fn [acc [x sin-modifier]]
                      (conj acc (+ (peek acc) (* base-distance (/ 1 sin-modifier)))))
                    [0]
                    (map (fn [x sin-modifier]
                           [x sin-modifier])
                         (take 160 (iterate (fn [x] (+ 10 x)) 0))
                         (cycle sins)))]

    (q/line x 0 x 800))

  (doseq [y (reduce (fn [acc [y sin-modifier]]
                      (conj acc (+ (peek acc) (* base-distance (/ 1 sin-modifier)))))
                    [0]
                    (map (fn [y sin-modifier]
                           [y sin-modifier])
                         (take 100 (iterate (fn [y] (+ 10 y)) 0))
                         (cycle sins)))]

    (q/line 0 y 1200 y))

  )

(try (q/defsketch example
       :title "waves1"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :svg
       ;; :output-file "waves1.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
