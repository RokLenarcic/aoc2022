(ns aoc.aoc9
  (:require [clojure.string :as str]
            [aoc.arrays :refer [p-sum]]))

(defn parse-input [s]
  (->> (str/trim s)
       (str/split-lines)
       (map #(str/split % #" "))
       (map (fn [[dir cnt]]
              [(case dir "L" [-1 0] "R" [1 0] "U" [0 -1] "D" [0 1])
               (parse-long cnt)]))))

(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc9.txt")))

(defn follow
  "Adjusts tail if needed to follow head"
  [[tx ty] [hx hy]]
  (let [unit #(Long/signum %)
        dx (- hx tx)
        dy (- hy ty)]
    (if (and (<= (abs dx) 1) (<= (abs dy) 1))
      [tx ty]
      [(+ tx (unit dx)) (+ ty (unit dy))])))

(def input-as-single-moves (mapcat (fn [[dir cnt]] (repeat cnt dir)) input))
(def head-movements (reductions p-sum [0 0] input-as-single-moves))
(def followers (iterate (partial reductions follow [0 0]) head-movements))

(defn p [n] (count (distinct (nth followers n))))
(comment (p 1) (p 9))
