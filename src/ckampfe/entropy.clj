(ns ckampmfe.elle.entropy
  (:require [quil.core :as q]))

(def width 1280)
(def height 800)

(defn draw []
  (q/stroke 200 200 200 )
  (q/fill 200 200 200)
  (q/stroke-weight 2)

  (let [yinterval 60
        yzero yinterval
        xinterval 60
        entropy (atom 1)
        ]

    (doseq [ystart (range yzero (- height yinterval) yinterval)]
      (let [x (atom xinterval)
            y (atom ystart)
            e2 (atom 1)]
        (swap! entropy (partial + 1))
        (while (< @x (- width (* 2.3 xinterval)))
          (q/line @x
                  @y
                  (swap! x (partial + 3))
                  (swap! y
                         #_+
                         (if (> (rand) 0.5) + -)
                         (if (and (> (rand) 0.8)
                                  (> @x @y))
                           (* (rand) (* @entropy @entropy @e2 0.00065))
                           0.5
                           )



                         ))
          (swap! e2 inc)
          ))))


  )

(defn setup []
  (q/background 255)
  (q/no-loop))


(q/defsketch example
  :title "entropy"
  :settings #(q/smooth 8)
  :setup setup
  :draw draw
  :size [1200 800]
  ;; :renderer :svg
  ;; :output-file "entropylines.svg"
  :features [:no-bind-output])
