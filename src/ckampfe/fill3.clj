(ns ckampfe.fill3
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
  (let [options (concat (repeat 10 [(+ x amount) y])
                        (repeat 1 [x (+ y amount)])
                        (repeat 10 [(- x amount) y])
                        (repeat 1 [x (- y amount)]) )
        options-set (set options)]
    (loop [trial (rand-nth options)
           seen #{trial}]
      (if (= seen options-set)
        nil
        (if (and (not (contains? pathset trial))
                 (not (out-of-bounds trial)))
          trial
          (let [trial (rand-nth options)
                seen (conj seen trial)]
            (recur trial seen)))))))

(defn draw-line
  "paths is a set of a map of {:vec [[x y] ...] and :set #{[x y]}}"
  [amount paths]
  (loop [paths paths]
    (let [previous (last (:vec paths))
          pathset (:set paths)
          next (try-next amount previous pathset)]
      (if next
        (-> paths
            (update :vec #(conj % next))
            (update :set #(conj % next)))
        (recur (-> paths
                   (update :vec pop)
                   (update :set #(disj % previous))))))))

(defn draw []
  (let [lines (take-while (fn [lines]
                            #_(println lines)
                            #_(println "item count" (count (:vec lines)))
                            (< (count (:vec lines))
                               120))
                          (iterate (partial draw-line 10)
                                   {:vec [[(* width 2/4) (* height 2/4)]]
                                    :set #{[(* width 2/4) (* height 2/4)]}}))
        linevec (:vec (last lines))
        pairs (partition 2 1 linevec)]
    (println "pairs" (count pairs))
    (doseq [[[x1 y1] [x2 y2]] pairs]
      (q/line x1 y1 x2 y2))))

(try (q/defsketch example
       :title "fill1"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :svg
       ;; :output-file "rot1.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
