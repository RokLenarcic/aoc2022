(ns aoc.aoc3
  (:require [clojure.java.io :as io]
            [aoc.colls :as c]))

(def priority
  (zipmap (concat "abcdefghijklmnopqrstuvwxyz"
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
          (range 1 53)))

(def input
  (map #(split-at (/ (count %) 2) %)
       (line-seq (io/reader "/Users/roklenarcic/aoc/aoc3_1.txt"))))

(defn char-in-all [colls]
  (if (next colls)
    (keep (into #{} (first colls))
          (char-in-all (next colls)))
    (first colls)))

(defn p1 [] (c/sum-of (fn [[s1 s2]] (priority (first (char-in-all [s1 s2])))) input))

(defn p2 []
  (c/sum-of (fn [[items1 items2 items3]]
              (priority (first (char-in-all [(flatten items1) (flatten items2) (flatten items3)]))))
            (partition-all 3 input)))
