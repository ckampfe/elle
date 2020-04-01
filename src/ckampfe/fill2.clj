(ns ckampfe.fill2
  (:require [quil.core :as q]
            [clojure.set :as set]))

(def width 1200)
(def height 800)

(defn setup []
  (q/background 255)
  (q/no-fill)
  (q/no-loop))

(defn out-of-bounds [[x y]]
  (or (<= x 0)
      (>= x width)
      (<= y 0)
      (>= y height)))

(defn try-next [amount [x y] pathset]
  (let [trial [(+ x amount) y]]
    (if-not (or (contains? pathset trial)
                (out-of-bounds trial))
      trial
      (let [trial [x (+ y amount)]]
        (if-not (or (contains? pathset trial)
                    (out-of-bounds trial))
          trial
          (let [trial [(- x amount) y]]
            (if-not (or (contains? pathset trial)
                        (out-of-bounds trial))
              trial
              (let [trial [x (- y amount)]]
                (if-not (or (contains? pathset trial)
                            (out-of-bounds trial))
                  trial
                  nil)))))))))

(defn draw-line
  "paths is a set of a map of {:vec [[x y] ...] and :set #{[x y]}}"
  [amount paths]
  (let [previous (last (:vec paths))
        pathset (:set paths)]
    (if-let [next (try-next amount previous pathset)]
      (do
        (-> paths
           (update :vec #(conj % next))
           (update :set #(conj % next)))))))

(defn draw []
  (let [lines (take 2286 (iterate (partial draw-line 20)
                               {:vec [[(* width 3/4) (* height 3/4)]]
                                :set #{[(* width 3/4) (* height 3/4)]}}))]
    (q/begin-shape :lines)
    (doseq [[[x1 y1] [x2 y2]] (partition 2 1 (:vec (last lines)))]
      (q/vertex x1 y1)
      (q/vertex x2 y2)
      #_(q/line x1 y1 x2 y2))
    (q/end-shape)))

(try (q/defsketch example
       :title "fill1"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :svg
       ;; :output-file "fill2.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
