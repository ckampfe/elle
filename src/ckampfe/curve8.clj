(ns ckampfe.curve8
  (:require [quil.core :as q]))

(defn setup []
  (q/background 255)
  (q/no-fill)
  (q/no-loop))

(defn draw []
  (doseq [x (range 40 800 170)]
    (let [stroke-weight-denominator (+ 1.2 (rand))]
      (doseq [n (range 0 500 20)]
        (q/stroke-weight (/ (Math/sqrt n)
                            stroke-weight-denominator))
        (q/line (+ 150 x) (+ 150 n)
                (+ 300 x) (+ 150 n))))))

(try (q/defsketch example
       :title "meghan1"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       :renderer :svg
       :output-file "hull8_curved.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))


;; Clojure

;; order
;; reference (naming). this name => that thing x = 1
;; x = 1
;; x = 2
;; x = 3.......
;; looping/repetition




























