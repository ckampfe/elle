(ns elle.core
  (:require [instaparse.core :as insta])
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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
