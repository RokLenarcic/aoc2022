(ns aoc.aoc6
  (:require [clojure.string :as str]))

(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc6_1.txt")))
(def test-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(defn marker-if-nonequal [c & items]
  (when (= (count items) (count (set items)))
    (+ (count items) c)))

(defn non-equal-characters [in n]
  (->> (iterate rest in)
       (take n)
       (apply map marker-if-nonequal (range))
       (remove nil?)))

(defn p1 [] (first (non-equal-characters input 4)))

(defn p2 [] (first (non-equal-characters input 14)))
