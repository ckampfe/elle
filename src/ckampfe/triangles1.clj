(ns ckampfe.triangles1
  (:require [quil.core :as q]))

(defn lines [triangle]
  (let [[v1 v2 v3] (:vertices triangle)]
    [[v1 v2]
     [v1 v3]
     [v2 v3]]))

(defn setup []
  (q/background 255)
  (q/no-fill)
  (q/no-loop))

(defn draw []
  (let [mesh (reduce (fn [acc n]
                       acc)
                     {:triangles [{:vertices [{:x 400 :y 400}
                                              {:x 410 :y 410}
                                              {:x 400 :y 415}]}]}
                     (range 10))]

    (doseq [triangle (:triangles mesh)
            [v1 v2] (lines triangle)]
      (q/line (:x v1) (:y v1) (:x v2) (:y v2)))))

(try (q/defsketch example
       :title "pendulums"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :svg
       ;; :output-file "triangles1.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
