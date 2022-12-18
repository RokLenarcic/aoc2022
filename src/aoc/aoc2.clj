(ns aoc.aoc2
  (:require [aoc.colls :as c]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (map #(str/split % #" +") (line-seq (io/reader "/Users/roklenarcic/aoc/aoc2_1.txt"))))

(def translate-val1 (fn [_ me] ({"X" "A" "Y" "B" "Z" "C"} me)))
(def translate-val2 (fn [opp me]
                      (get
                        (case me
                          "X" {"A" "C" "B" "A" "C" "B"}
                          "Y" {"A" "A" "B" "B" "C" "C"}
                          "Z" {"A" "B" "B" "C" "C" "A"})
                        opp)))
(def pick-val {"A" 1 "B" 2 "C" 3})
(def match-val {["A" "B"] 6 ["B" "C"] 6 ["C" "A"] 6
                ["B" "A"] 0 ["C" "B"] 0 ["A" "C"] 0
                ["A" "A"] 3 ["B" "B"] 3 ["C" "C"] 3})

(defn grade-round
  [[opp me] translate-me]
  (let [me* (translate-me opp me)]
    (+ (pick-val me*) (match-val [opp me*]))))

(defn p1 []
  (c/sum-of #(grade-round % translate-val1) input))

(defn p2 [] (c/sum-of #(grade-round % translate-val2) input))
