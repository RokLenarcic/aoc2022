(ns aoc.aoc13
  (:require
    [aoc.colls :as c]
    [aoc.inputs :as inputs]
    [medley.core :as m]))

(def test-input (inputs/blocks "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]" read-string))
(def input (inputs/blocks (slurp "/Users/roklenarcic/aoc/aoc13.txt") read-string))

(defn as-vec [x] (if (vector? x) x [x]))

(defn c [x y]
  (if (and (number? x) (number? y))
    (- x y)
    (let [x (as-vec x) y (as-vec y)]
      (or (m/find-first (complement zero?) (map c x y))
          (- (count x) (count y))))))

(defn p1 [input]
  (c/sum-of (keep-indexed (fn [idx [x y]]
                            (when (neg-int? (c x y))
                              (inc idx))) input)))

(defn p2 [input]
  (let [dividers #{[[2]] [[6]]}
        packets (sort-by identity c (into (apply concat input) dividers))]
    (->> (for [[idx packet] (m/indexed packets)
               :when (dividers packet)]
           (inc idx))
         (apply *))))
