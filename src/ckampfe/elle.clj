(ns ckampfe.elle
  (:require [quil.core :as q]
            [instaparse.core :as insta]
            [com.hypirion.clj-xchart :as x])
  (:gen-class))

(def algae-parser
  (insta/parser
   "<S> = (A | B)+
      A = <'a'>
      B = <'b'>"))

(def algae-transforms {:A (fn [] :AB)
                       :B (fn [] :A)})

(def dragon-parser
  (insta/parser
   "<S> = (X | Y | ANYTHING)+
      X = <'x'>
      Y = <'y'>
      ANYTHING = ('f' | '-' | '+')
      "))

(def dragon-transforms {:X (fn [] :X+YF+)
                        :Y (fn [] :-FX-Y)
                        :ANYTHING (fn [r] r)})

(defn parse-and-transform [parser transforms data]
  (->> (insta/parse parser data)
       (insta/transform transforms)
       (map (comp clojure.string/lower-case name))
       (clojure.string/join)))

(def retree-parser
  (insta/parser
   "<S> = (X | Y | T | ANYTHING)+
      X = <'x'>
      Y = <'y'>
      T = <'t'>
      ANYTHING = ('f' | 'r' | 'l' | '-' | '+' | 'z')
"))

(def retree-transforms
  {:X (fn [] :YFRTRY)
   :Y (fn [] :XFLFFLFR)
   :T (fn [] :XYYYY)
   :ANYTHING (fn [r] r)})

(def conc-parser
  (insta/parser
   "<S> = (X | Y )+
      X = <'x'>
      Y = <'y'>
"))

(def conc-transforms
  {:X (fn [] :Y)
   :Y (fn [] :X)})

(defn interp-conc [conc]
  (let [xsize (atom 50)
        ysize (atom 50)]
    (doseq [c conc]
      (let [c (.toString c)]
        (case c
          "x" (do
                (q/ellipse 600 400 @xsize @ysize)
                (swap! xsize (partial + (+ (rand-int 20) @ysize))))
          "y" (do
                (q/ellipse 600 400 @ysize @xsize)
                (swap! ysize (partial + (+ (rand-int 20) @ysize)))))))))

(comment

  (clojure.pprint/pprint (take 4 (iterate (partial parse-and-transform
                                                   conc-parser
                                                   conc-transforms)
                                          "x")))

  (clojure.pprint/pprint (take 4 (iterate (partial parse-and-transform
                                                   retree-parser
                                                   retree-transforms)
                                          "x"))))

(comment

  (insta/parse dragon-parser "fx")

  (clojure.pprint/pprint (take 8 (iterate (partial parse-and-transform
                                                   algae-parser
                                                   algae-transforms)
                                          "a")))

  (clojure.pprint/pprint (take 8 (iterate (partial parse-and-transform
                                                   dragon-parser
                                                   dragon-transforms)
                                          "fx")))


  ;; n = 0 : A
  ;; n = 1 : AB
  ;; n = 2 : ABA
  ;; n = 3 : ABAAB
  ;; n = 4 : ABAABABA
  ;; n = 5 : ABAABABAABAAB
  ;; n = 6 : ABAABABAABAABABAABABA
  ;; n = 7 : ABAABABAABAABABAABABAABAABABAABAAB 
  )

(defn interp-retree [dragon]
  (let [angle (atom 0)

        pos (atom {:x 500 :y 500})

        update-angle (fn [old-angle direction]
                       (case direction
                         "r" (if (= old-angle 270)
                               0
                               (+ old-angle 90))
                         "l" (if (= old-angle 0)
                               270
                               (- old-angle 90))))
        line-inc 5

        draw-line (fn [this-pos angle]
                    (case angle
                      90 (do (q/line (:x this-pos)
                                     (:y this-pos)
                                     (+ (:x this-pos) line-inc)
                                     (:y this-pos))
                             (update this-pos :x (fn [x] (+ x line-inc))))

                      180 (do (q/line (:x this-pos)
                                      (:y this-pos)
                                      (:x this-pos)
                                      (+ (:y this-pos) line-inc))
                              (update this-pos :y (fn [y] (+ y line-inc))))

                      270 (do (q/line (:x this-pos)
                                      (:y this-pos)
                                      (- (:x this-pos) line-inc)
                                      (:y this-pos))
                              (update this-pos :x (fn [x] (- x line-inc))))

                      0 (do (q/line (:x this-pos)
                                    (:y this-pos)
                                    (:x this-pos)
                                    (- (:y this-pos) line-inc))
                            (update this-pos :y (fn [y] (- y line-inc))))))]
    (doseq [letter dragon]
      (let [letter (.toString letter)]
        #_(println letter)
        #_#(println @angle)
        (case letter
          "f" (let [new-pos (draw-line @pos @angle)]
                #_(println @pos)
                #_(println new-pos)
                (reset! pos new-pos))
          "r" (swap! angle update-angle letter)
          "l" (swap! angle update-angle letter)
          nil)))))

(defn interp-dragon [dragon]
  (let [angle (atom 0)

        pos (atom {:x 570 :y 110})

        update-angle (fn [old-angle direction]
                       (case direction
                         "+" (if (= old-angle 270)
                               0
                               (+ old-angle 90))
                         "-" (if (= old-angle 0)
                               270
                               (- old-angle 90))))
        line-inc 5

        draw-line (fn [this-pos angle]
                    (case angle
                      90 (do (q/line (:x this-pos)
                                     (:y this-pos)
                                     (+ (:x this-pos) line-inc)
                                     (:y this-pos))
                             (update this-pos :x (fn [x] (+ x line-inc))))

                      180 (do (q/line (:x this-pos)
                                      (:y this-pos)
                                      (:x this-pos)
                                      (+ (:y this-pos) line-inc))
                              (update this-pos :y (fn [y] (+ y line-inc))))

                      270 (do (q/line (:x this-pos)
                                      (:y this-pos)
                                      (- (:x this-pos) line-inc)
                                      (:y this-pos))
                              (update this-pos :x (fn [x] (- x line-inc))))

                      0 (do (q/line (:x this-pos)
                                    (:y this-pos)
                                    (:x this-pos)
                                    (- (:y this-pos) line-inc))
                            (update this-pos :y (fn [y] (- y line-inc))))))]
    (doseq [letter dragon]
      (let [letter (.toString letter)]
        #_(println letter)
        #_#(println @angle)
        (case letter
          "f" (let [new-pos (draw-line @pos @angle)]
                #_(println @pos)
                #_(println new-pos)
                (reset! pos new-pos))
          "+" (swap! angle update-angle letter)
          "-" (swap! angle update-angle letter)
          nil)))))

(partition 3 1 [:a :b :c :d :e :f :g :h :i])

(defn pattern-reduce [n p initial-col pattern-f]
  (take n (iterate (fn [old]
                     (let [nwise (partition p 1 (last old))]
                       (conj
                        old
                        (concat [false] (mapv pattern-f nwise)))))
                   initial-col)))

#_(defn rgb-cosine-similarity [v1 v2]
    (let [a1 (m/array v1)
          a2 (m/array v2)
          m1 (m/magnitude a1)
          m2 (m/magnitude a2)
          dot (m/dot a1 a2)]
      (/ dot (* m1 m2))))

(defn length [[point1 point2]]
  (Math/sqrt ^double (+ (Math/pow ^double (- (:x point1)
                                             (:x point2))
                                  2)
                        (Math/pow ^double (- (:y point1)
                                             (:y point2))
                                  2))))

(defn min-dist [edges]
  (reduce (fn [acc edge]
            (+ acc (length edge)))
          0.0
          edges))

(defn temperature
  "temperature falls as k-over-kmax increases"
  [k-over-kmax]
  (* 1 k-over-kmax))

(defn annealing-probability
  "temp is rational value between 1 and 0.

   When T tends to zero, the probability P(e,e',T) must tend to zero if e' > e and to a positive value otherwise.

  energy-s and energy-new-s are both floats, the consine sim of target and state (or new prospective state)"
  [energy-s energy-new-s temperature]
  (let [;; normalized (- energy-s energy-new-s)
        ]
    (if (> energy-s energy-new-s)
      1.1
      (- 1 (Math/pow (/ (- energy-s energy-new-s)
                        energy-s)
                     temperature)))))

(defn random-new-neighbor [s temperature energy-fn]
  (let [s-energy (energy-fn s)
        massaged (set (flatten s))
        shuffled (shuffle massaged)
        s-new (filter (fn [edge]
                        (> (count edge) 1))
                      (map vec (partition-all 2 1 shuffled)))

        s-new-energy (energy-fn s-new)]
    s-new

    #_(loop [[s-new-energy s-new] [(energy-fn s-new) s-new]]

        (if (>= (/ (Math/abs ^double (- s-new-energy s-energy))
                   s-energy)
                0.15)
          (do
            #_(println (/ (Math/abs ^double (- s-new-energy s-energy))
                          s-energy))
            (recur
             (let [shuffled (shuffle massaged)
                   s-new (filter (fn [edge]
                                   (> (count edge) 1))
                                 (map vec (partition-all 2 1 shuffled)))

                   s-new-energy (energy-fn s-new)]
               [s-new-energy s-new])))
          s-new))))

(defn simulated-annealing
  "When T tends to zero, the probability P(e,e',T) must tend to zero if e' > e and to a positive value otherwise."
  [initial-s kmax]
  (let [energy-fn min-dist]
    (reduce (fn [[acc s] k]
              (let [temperature (temperature (- 1 (/ k kmax)))
                    s-new (random-new-neighbor s temperature energy-fn)
                    this-rand (rand)]

                (if (and (> (annealing-probability (energy-fn s)
                                                   (energy-fn s-new)
                                                   temperature)
                            this-rand)
                         (not= s (reverse s-new))
                         (not= s (rseq (vec s-new)))
                         (not= s s-new)
                         (not= (energy-fn s)
                               (energy-fn s-new)))
                  [(conj acc [s-new (energy-fn s-new)])
                   s-new]
                  [acc s])))
            [[] initial-s]
            (range kmax))))

(defn initialize-points [number-of-points x1 y1 x2 y2]
  "upper left and lower right, respectively"
  (vec (for [_ (range number-of-points)]
         {:x (rand (- x2 x1))
          :y (rand (- y2 y1))})))

(defn initialize-points2 [number-of-points x1 y1 x2 y2]
  (let [magnitudes [#_0.85 #_0.7 0.65 #_0.45 0.3 0.15 #_0.1]
        adjusted-magnitudes (mapcat (fn [m] [(- 1 m)
                                             (Math/abs ^double (- m 1))])
                                    magnitudes)]
    (mapcat (fn [[mag1 mag2]]
              [;; {:x (* mag1 (- x2 x1))
               ;;  :y (* mag1 (- y2 y1))}

               {:x (* mag2 (- x2 x1))
                :y (* mag1 (- y2 y1))}

               {:x (* mag1 (- x2 x1))
                :y (* mag2 (- y2 y1))}

               {:x (* mag2 (- x2 x1))
                :y (* mag2 (- y2 y1))}

               {:x (* mag1 x1)
                :y (* mag2 y1)}])
            (partition 2 1 adjusted-magnitudes))))

(defmacro xor
  ([] false)
  ([a] a)
  ([a b]
   `(let [a# ~a
          b# ~b]
      (if a#
        (if b# false a#)
        (if b# b# false)))))

(defn setup []
  (q/background 255)
  (q/no-stroke)
  (q/no-loop))

(defn f1 [[a b c]]
  (or (and (xor a b)
           (not c))
      (not a)))

(defn f2 [[a b c]]
  (or (and (xor a b)
           (not c))
      (not c)))

(defn f3 [[a b c]]
  (or (xor a b)
      (not c)))

(defn f4 [[a b c]]
  (not (or (xor a b)
           c)))

(defn f5 [[a b c]]
  (xor
   (xor b c) (and a c)))

(defn f6 [[a b c]]
  (xor
   (or a c)
   (or a b)))

(defn f7 [[b a c]]
  (xor
   (or a c)
   (or a b)))

(defn f8 [[b a c]]
  (xor
   (if (< 0.19 (rand))
     c)
   (not c)))

(defmacro nor [a b]
  `(and (not ~a)
        (not ~b)))

(defmacro nand [a b]
  `(not (and ~a ~b)))

(defn f9 [[a b c]]
  (nor b (xor a c)))

(defn f10 [[a b c]]
  (nor c (xor a b)))

(defn f11 [[a b c]]
  (nor a (xor b c)))

(defn f12 [[a b c]]
  (xor (xor a c)
       (nor b c)))

(defn f13 [[a b c]]
  (nand (xor a c)
        (nand b c)))

(defn f14 [[a b c]]
  (xor (xor a c)
       (nor b c)))

(defn draw []
  #_(q/no-fill)
  #_(q/no-stroke)
  (q/fill 200 200 200)
  #_(interp-conc (take 20 (iterate (partial parse-and-transform
                                            conc-parser
                                            conc-transforms)
                                   "x")))

  #_(let [sims (simulated-annealing (random-new-neighbor (partition 2 1 (initialize-points 8 10 10 110 80)) 1.0)
                                    1000)]
      (println (count (first sims)))
      (doall (map-indexed (fn [k part]
                            (doall (map-indexed (fn [j s]
                                                  #_(println (count s))
                                                  #_(q/text (str (Math/round (min-dist s)))
                                                            (* (+ j 1) 55)
                                                            (* (+ k 1) 105))
                                                  (doall (map-indexed (fn [i [{x :x y :y}
                                                                              {x2 :x y2 :y}]]
                                                                        (let [adj (* (+ j 1) 120)
                                                                              adjy (* (+ k 1) 110)]
                                                                          (q/ellipse (+ adj x) (+ adjy y)
                                                                                     2 2)
                                                                          (q/ellipse (+ adj x2) (+ adjy y2)
                                                                                     2 2)
                                                                          (q/line (+ adj x) (+ adjy y) (+ adj x2) (+ adjy y2))))
                                                                      s)))
                                                part)))
                          (partition 8 (reverse (take 50 (reverse (first sims))))))))

  (let [sims (simulated-annealing (random-new-neighbor
                                   #_(partition 2 1 (initialize-points2 5 60 60 210 180))
                                   (partition 2 1 (initialize-points 9 70 60 280 270))
                                   1.0
                                   min-dist)
                                  10000)
        ;; indexed (map-indexed (fn [i [s e]]
        ;;                        {:x i
        ;;                         :y e})
        ;;                      sims)
        ;; indexed (apply merge-with conj indexed)
        ]

    #_(x/view
       (x/xy-chart
        {"energy" {:x (range (count (map second (first sims))))
                   :y (map second (first sims))}}
        {:title "ok"}))

    (println (count (first sims)))
    (doall (map-indexed (fn [k part]
                          (doall (map-indexed (fn [j [s e]]
                                                (let [adj (* (inc j) 230)
                                                      adjy (* (inc k) 210)]
                                                ;; (let [adj (* (+ j 1) 120)
                                                ;;       adjy (* (+ k 1) 110)]
                                                  #_(q/text (apply str (take 3 (.toString e))) adj adjy)
                                                  (doall (map-indexed (fn [i [{x :x y :y}
                                                                              {x2 :x y2 :y}]]
                                                                        ;; (let [adj (* (+ j 1) 120)
                                                                        + adj ;;       adjy (* (+ k 1) 110)]
                                                                        #_(q/ellipse (+ adj x) (+ adjy y)
                                                                                     2 2)
                                                                        #_(q/ellipse (+ adj x2) (+ adjy y2)
                                                                                     2 2)
                                                                        (q/curve

                                                                         (+ (* 70 (rand)) (+ adj x))
                                                                         (+ (* 70 (rand)) (+ adjy y))
                                                                         (+ adj x)
                                                                         (+ adjy y)
                                                                         (+ adj x2)
                                                                         (+ adjy y2)
                                                                         (+ (* 70 (rand)) (+ adj x2))
                                                                         (+ (* 70 (rand)) (+ adjy y2)))
                                                                        ;; )
                                                                        )

                                                                      s))))
                                              part)))
                        (take 3 (partition-all 4 (reverse (take 50 (reverse (first sims)))))))))

  #_(doall (map-indexed (fn [i row]
                          (doall (map-indexed (fn [j column]
                                                (let [x (* 10 (+ 1 (- 117 j)))
                                                      y (* 10 (+ 1 (- 78 i)))]
                                                  (when (and column
                                                             (< x (- 1200 20))
                                                             (> x 20)
                                                             (< y (- 800 20))
                                                             (> y 20))
                                                    #_(q/line (* 10 (+ 1 (- 117 j)))
                                                              (* 10 (+ 1 (- 78 i)))
                                                              (* 10 (+ 1 (- 117 j)))
                                                              (+ 5 (* 10 (+ 1 (- 78 i))))
                                                              #_10
                                                              #_10)
                                                    (q/rect (* 10 (+ 1 (- 117 j)))
                                                            (* 10 (+ 1 (- 78 i)))
                                                            10
                                                            10))))
                                              row)))

                        (last (pattern-reduce 78 3
                                              [[true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true]]
                                              f14))))
  #_(doall (map-indexed (fn [i row]
                          (doall (map-indexed (fn [j column]
                                                (let [x (* 10 (+ 1 j))
                                                      y (* 10 (+ 1 i))]
                                                  (when (and column
                                                             (< x (- 1200 20))
                                                             (> x 20)
                                                             (< y (- 800 20))
                                                             (> y 20))
                                                    (q/rect (* 10 (+ 1 j))
                                                            (* 10 (+ 1 i))
                                                            10
                                                            10))))
                                              row)))

                        (last (pattern-reduce 78 3
                                              [[true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true
                                                true false false true false true true true false true false false true true]]
                                              f8))))

  (comment (interp-dragon (last (take 15 (iterate (partial parse-and-transform
                                                           dragon-parser
                                                           dragon-transforms)
                                                  "fx")))))

  (comment (interp-retree (last (take 16 (iterate (partial parse-and-transform
                                                           retree-parser
                                                           retree-transforms)
                                                  "x"))))))

(try (q/defsketch example
         :title "elle"
         :settings #(q/smooth 1)
         :setup setup
         :draw draw
         :size [1200 800]
         :renderer :svg
         :output-file "shortpath6.svg"
         :features [:no-bind-output])
       (catch Exception e
         (println e)))

(defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "Hello, World!"))
