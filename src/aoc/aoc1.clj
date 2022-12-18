(ns aoc.aoc1
  (:require [aoc.inputs :as in]
            [aoc.colls :as c]))

(defn elves-calories []
  (map c/sum-of (in/blocks (slurp "/Users/roklenarcic/aoc/aoc1_1.txt") parse-long)))

(defn a1 []
  (apply max (elves-calories)))

(defn a2 []
  (c/sum-of (take 3 (reverse (sort (elves-calories))))))
