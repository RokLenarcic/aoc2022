(ns aoc.aoc8
  (:require [aoc.inputs :as inputs]
            [aoc.arrays :as arrays]))

(def input (inputs/char-array2d (slurp "/Users/roklenarcic/aoc/aoc8.txt") (comp parse-long str)))
(def test-input (inputs/char-array2d "30373\n25512\n65332\n33549\n35390" (comp parse-long str)))

(defn visible? [input p]
  (let [height (get-in input p)]
    (not-empty
      (for [dir arrays/dirs
            :when (every? #(< % height) (rest (arrays/vray input dir p)))]
        true))))

(defn viewing-distance [arr rc coord-run]
    (let [height (get-in arr rc)]
      (reduce
        (fn [c rc]
          (if (<= height (get-in arr rc))
            (reduced (inc c))
            (inc c)))
        0
        coord-run)))

(defn scenic-score [arr rc]
  (apply * (map #(viewing-distance arr rc %)
                (arrays/fan-out arr arrays/dirs rc))))

(defn p1 []
  (count (filter (partial visible? input)
                 (keys (arrays/filter-arr (arrays/->ArrayKd input arrays/dirs) any?)))))

(defn p2 []
  (apply max
         (for [row (range (count input))
               col (range (count (input row)))]
           (scenic-score input [row col]))))
