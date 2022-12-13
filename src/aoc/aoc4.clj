(ns aoc.aoc4
  (:require [aoc.inputs :as inputs]
            [clojure.java.io :as io]))

(defn contains [small big]
  (and (<= (:start big) (:start small))
       (>= (:end big) (:end small))))

(defn overlaps? [s1 s2]
  (if (< (:start s1) (:start s2))
    (<= (:start s2) (:end s1))
    (<= (:start s1) (:end s2))))

(def input
  (->> "/Users/roklenarcic/aoc/aoc4_1.txt"
       (io/reader )
       line-seq
       (map inputs/parse-numbers)
       (map (fn [[b1 e1 b2 e2]]
              [{:start b1 :end e1}
               {:start b2 :end e2}]))))

(defn p1 []
  (->> input
       (filter (fn [[s1 s2]]
                 (or (contains s1 s2) (contains s2 s1))))
       count))

(defn p2 []
  (->> input
       (filter (fn [[s1 s2]]
                 (overlaps? s1 s2)))
       count))
