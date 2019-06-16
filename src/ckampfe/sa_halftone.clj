(ns ckampfe.sa-halftone)

(defn p [tmax time e e']
  (if (> e' e)
    (/ (* time (Math/sqrt (if (<= (- e' e) 1)
                     0.1
                     (- e' e))))
       (* tmax (- e' e)))
    1))

(defn anneal [initial-time tick-size initial-state state->state' e-fn p-fn]
  (loop [time initial-time
         state initial-state
         states []]
    (if (<= time 0)
      [state states]
      (let [state' (state->state' state)
            e (e-fn state)
            e' (e-fn state')
            probability (p-fn initial-time time e e')
            new-state (if (< probability (rand))
                        state
                        state')]

        (println {:tmax initial-time
                  :time time
                  :e e
                  :eprime e'
                  :probability probability
                  :oldstate state
                  :newstate new-state})

        (recur (- time tick-size)
               new-state
               (conj states new-state))))))

(anneal 100 ;; initial-time
        1 ;; tick-size
        5 ;; initial-state
        ;; state->state' fn
        (fn [state]
          (if (< (rand) 0.5)
            (dec state)
            (inc state)))
        (fn [state] (Math/abs (- state 50))) ;; e-fn
        p ;; p-fn
        )

#_(defn p [tmax time e e']
    (if (> e' e)
      (/ (* time (- e' e))
         (* tmax (- e' e)))
      1))

(Math/sqrt (- 62 61))

(p 100 18 61 62)

