(ns ckampfe.llll
  (:require [instaparse.core :as insta]
            [quil.core :as q]
            [clojure.java.io :as io]))

(def g
  (insta/parser
   "<S> = (a | b | c)+
    a = <'a'>
    b = <'b'>
    c = <'c'>
"))

(def t
  {:a (fn [] :abc)
   :b (fn [] :ca)
   :c (fn [] :ac)})

#_(def p
    (insta/parse g "ab"))

#_(insta/transform t (insta/parse g "abbb"))

(defn f [seed n]
  (take n (iterate (fn [old]
                     (let [res (insta/parse g old)
                           transformed (insta/transform t res)
                           as-str (apply str (map name transformed))]
                       as-str))
                   seed)))

#_(f "ab" 5)

(defn setup []
  (q/stroke-weight 6)
  (q/background 255)
  (q/no-fill)
  (q/no-loop))

(defn draw []
  (let [instructions (last (f "babb" 7))
        state (atom {:current-point {:x 600 :y 400}
                     :n 15
                     :direction [:up-left :down-left :up-right :down-right]})]
    (doseq [c instructions]
      #_(println c)

      ;; change direction
      (when (= c \b)
        (swap! state
               (fn [s]
                 (update s :direction
                         (fn [old]
                           (conj (vec (drop 1 old))
                                 (first old)))))))

      (let [direction (first (:direction @state))
            direction-2 (first (drop 1 (:direction @state)))
            old-point (:current-point @state)
            n (:n (swap! state (fn [s]
                                 (update s :n (fn [n]
                                                (+ n
                                                   #_0.001
                                                   #_(/ (Math/sqrt n)
                                                        n)))))))
            new-point-1
            (case direction
              :down-right (-> old-point
                              (update  :y (partial + n))
                              (update :x (partial + n)))
              :down-left (-> old-point
                             (update :x (fn [x] (- x n)))
                             (update :y (fn [y] (+ y n))))
              :up-right (-> old-point
                            (update :y (fn [y] (- y n)))
                            (update :x (fn [x] (+ x n))))
              :up-left (-> old-point
                           (update :y (fn [y] (- y n)))
                           (update :x (fn [x] (- x n)))))

            new-point-2
            (case direction-2
              :down-right (-> old-point
                              (update :y (partial + n))
                              (update :x (partial + n)))
              :down-left (-> old-point
                             (update :y (fn [y] (+ y n)))
                             (update :x (fn [x] (- x n))))
              :up-right (-> old-point
                            (update :y (fn [y] (- y n)))
                            (update :x (fn [x] (+ x n))))
              :up-left (-> old-point
                           (update :y (fn [y] (- y n)))
                           (update :x (fn [x] (- x n)))))]

        (reset! state (assoc @state :current-point new-point-1))

        (q/line (:x old-point)
                (:y old-point)
                (:x new-point-1)
                (:y new-point-1))

        (when (= c \c)
          (q/line (:x old-point)
                  (:y old-point)
                  (:x new-point-2)
                  (:y new-point-2)))

        #_(q/line 20 20 30 30)
        #_(println direction)))))

(try (q/defsketch example
       :title "ok"
       :settings #(q/smooth 2)
       :setup setup
       :draw draw
       :size [1200 800]
       ;; :renderer :svg
       ;; :output-file "l1.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))

#_(defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "Hello, World!"))

(require '[clojure.data.xml :refer :all])

(def lines
  (let [input-xml (io/reader (io/file "/Users/clark/code/ckampfe.elle/l1_lines.svg"))
        g (parse input-xml)
        lines (:content g)]
    (filter #(not= % "\n")
            lines)))

(count lines)
;; => 1055


(first lines)
;; => #xml/element{:tag :line, :attrs {:y2 "385", :style "fill:none;", :x1 "600", :x2 "585", :y1 "400"}}

(defn sort-xys [line-xml]
  (let [[x1 x2 y1 y2] ((juxt :x1 :x2 :y1 :y2) (:attrs line-xml))
        [x1 x2 y1 y2] (map #(Integer. %) [x1 x2 y1 y2])
        [x1 x2 y1 y2] (if (or (<= x1 x2)
                              #_(<= y1 y2))
                        [x1 x2 y1 y2]
                        [x2 x1 y2 y1])

        kvs {:x1 x1 :x2 x2 :y1 y1 :y2 y2}]

    (reduce (fn [acc [k v]]
              (assoc-in acc
                        [:attrs k]
                        v)) line-xml kvs)))

(sort-xys (first lines))

(def simp-dedup-lines (into #{} lines))

(count (into #{} (pmap sort-xys lines)))

(def unique-lines (into #{} (pmap sort-xys lines)))

#_(with-open [out-file (java.io.FileWriter. "/Users/clark/code/ckampfe.elle/l1_lines_dedup.svg")]
    (doseq [line unique-lines]
      (emit line out-file)))

(defn group-by-slope [lines]
  (group-by (fn [line]
              (/ (- (-> line :attrs :y2)
                    (-> line :attrs :y1))
                 (- (-> line :attrs :x2)
                    (-> line :attrs :x1))))
            lines))

(defn sort-by-x [lines]
  (sort-by (fn [line]
             (Integer. (-> line :attrs :x1)))
           lines))

(defn merge-colinear-lines [lines]
  (let [ordered (map sort-xys lines)
        by-slopes (group-by-slope ordered)
        sorted-by-x (->> by-slopes
                         (map (fn [[slope line-segs]]
                                [slope (sort-by-x line-segs)]))
                         (into {}))
        merged (->> sorted-by-x
                    (map (fn [[slope line-segs]]
                           [slope (vals (reduce (fn [acc val]
                                             (let [my-x1-y1 ((juxt :x1 :y1) (:attrs val))
                                                   my-x2-y2 ((juxt :x2 :y2) (:attrs val))]
                                               (if-let [acc-line (get acc my-x1-y1)]
                                                 ;; true: add me to existing line
                                                 (-> acc
                                                     (assoc my-x2-y2
                                                            (-> acc-line
                                                                (assoc-in [:attrs :x2] (-> val :attrs :x2))
                                                                (assoc-in [:attrs :y2] (-> val :attrs :y2))))
                                                     (dissoc my-x1-y1))
                                                 ;; false: I am the start of a line
                                                 (assoc acc my-x2-y2 val))))
                                           {}
                                           line-segs))]))
                    (into {}))]
    merged))

(clojure.pprint/pprint (into {}
                             (map (fn [[k v]]
                                    [k (take 3 v)])
                                  (merge-colinear-lines unique-lines))))

(def merged-lines (mapcat (fn [[slope lines]]
                            lines)
                          (merge-colinear-lines unique-lines)))

(with-open [out-file (java.io.FileWriter. "/Users/clark/code/ckampfe.elle/l1_lines_dedup_merged.svg")]
    (doseq [line merged-lines]
      (emit line out-file)))






