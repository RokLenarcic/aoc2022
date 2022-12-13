(ns aoc.aoc1
  (:require [aoc.inputs :as in]))

(defn elves-calories []
  (map #(apply + %) (in/blocks (slurp "/Users/roklenarcic/aoc/aoc1_1.txt") parse-long)))

(defn a1 []
  (apply max (elves-calories)))

(defn a2 []
  (apply + (take 3 (reverse (sort (elves-calories))))))
