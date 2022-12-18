(ns aoc.aoc20
  (:require [clojure.string :as str])
  (:import (java.util ArrayList)))

(def test-input (mapv parse-long (str/split-lines "1\n2\n-3\n3\n-2\n0\n4")))
(def input (mapv parse-long (str/split-lines (slurp "/Users/roklenarcic/aoc/aoc20.txt"))))

(defn insert-pos
  "Insert position after removal of an element"
  [prev delta size]
  (let [new-pos (mod (+ prev delta) size)]
    (if (zero? new-pos)
      (if (zero? prev) 0 size)
      new-pos)))

(defn move-nth [^ArrayList els i]
  (let [[at n] (first (keep-indexed (fn [new-idx n]
                                      (when (= (first n) i) [new-idx n]))
                                    els))]
    (.remove els ^int at)
    (.add els (insert-pos at (second n) (.size els)) n)))

(defn mix [els] (doseq [i (range (.size els))] (move-nth els i)))
(defn init-list [in]
  (let [indices (ArrayList.)]
    (doseq [i (range (count in))] (.add indices [i (in i)]))
    indices))

(defn get-the-number [mixed-indices]
  (let [c (drop-while #(not= % 0) (cycle (map second mixed-indices)))]
    (transduce (map (partial nth c)) + [1000 2000 3000])))

(defn p1 [in]
  (let [indices (init-list in)]
    (mix indices)
    (get-the-number indices)))

(defn p2 [in]
  (let [indices (init-list (mapv (partial * 811589153) in))]
    (dotimes [_ 10] (mix indices))
    (get-the-number indices)))
