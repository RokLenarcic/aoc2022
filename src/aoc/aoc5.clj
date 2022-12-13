(ns aoc.aoc5
  (:require [aoc.inputs :as inputs]
            [aoc.arrays :as arrays]))

(def raw-input
  (inputs/blocks
    (slurp "/Users/roklenarcic/aoc/aoc5_1.txt")
    identity))

(defn parse-stack-line
  [l]
  (map second (re-seq #"(?:   |\[(\w)\])(?: |$)" l)))

(defn parse-move
  [l]
  (-> (zipmap
        [:n :from :to]
        (map parse-long (rest (re-find #"move (\d+) from (\d+) to (\d+)" l))))
      (update :from dec)
      (update :to dec)))

(defn apply-move [keep-order? state {:keys [n from to]}]
  (let [crates (cond-> (take n (state (dec from)))
                 keep-order? reverse)]
    (-> state
        (update from #(drop n %))
        (update to into crates))))

(def initial-state
  (->> (butlast (first raw-input))
       (mapv parse-stack-line)
       arrays/rows-to-cols
       (mapv #(filter some? %))))

(def moves
  (->> (second raw-input)
       (map parse-move)))

(defn p1 []
  (->> (reduce (partial apply-move false) initial-state moves)
       (map first)
       (apply str)))

(defn p2 []
  (->> (reduce (partial apply-move true) initial-state moves)
       (map first)
       (apply str)))
