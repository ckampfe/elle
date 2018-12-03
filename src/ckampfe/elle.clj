(ns ckampfe.elle
  (:require [quil.core :as q]
            [instaparse.core :as insta])
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

(defn draw []
  #_(q/no-fill)
  (q/no-stroke)
  (q/fill 200 200 200)
  #_(interp-conc (take 20 (iterate (partial parse-and-transform
                                            conc-parser
                                            conc-transforms)
                                   "x")))
  (doall (map-indexed (fn [i row]
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
                                              true false false true false true true true false true false false true true
                                              ]]
                                            f3
                                            ))))

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
       :output-file "f3.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))

#_(defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "Hello, World!"))
