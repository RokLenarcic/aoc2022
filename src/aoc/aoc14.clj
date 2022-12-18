(ns aoc.aoc14
  (:require [clojure.string :as str]
            [aoc.arrays :as a]
            [aoc.colls :refer [iterate*]]
            [medley.core :as m]))

(defn parse-shape [l]
  (let [s (map #(map parse-long %) (re-seq #"(\d+),(\d+)" l))]
    (mapcat #(a/rasterize (rest %1) (rest %2)) s (rest s))))

(defn floor [in] (apply max (map second in)))

(defn parse-input [in]
  (let [basic (into #{} (mapcat parse-shape (str/split-lines in)))
        new-floor (+ 2 (floor basic))]
    (into basic (map #(vector % new-floor)) (range -1000 1000))))

(def test-input (parse-input "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc14.txt")))

(defn move-sand [state p]
  (m/find-first #(nil? (state %)) (a/adjacent* [[0 1] [-1 1] [1 1]] p)))

(defn trace-sand [state]
  (some->> (last (rest (iterate* (partial move-sand state) [500 0])))
           (conj state)))

(defn p1 [input]
  (let [on-floor-idx (dec (floor input))
        nothing-on-floor? (fn [state] (not-any? #(= (second %) on-floor-idx) state))]
    (dec (count (iterate* trace-sand nothing-on-floor? input)))))

(defn p2 [input] (count (iterate* trace-sand input)))
