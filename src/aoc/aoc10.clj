(ns aoc.aoc10
  (:require [aoc.colls :refer [map-of]]
            [aoc.inputs :as inputs]
            [clojure.string :as str]))

(def init-state {:x 1 :clk 1})

(defn parse-prog [s]
  (map
    #(inputs/reg-parse %
       [_ v] #"addx (-?\d+)" {:i :addx :v (parse-long v)}
       [_] #"noop" {:i :noop})
    (str/split-lines s)))

(def prog (parse-prog (slurp "/Users/roklenarcic/aoc/aoc10.txt")))
(def test-prog (parse-prog "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"))

(defn more-states [{:keys [x clk]} {:keys [i v]}]
  (if (= :noop i)
    [(map-of x (inc clk))]
    [(map-of x (inc clk)) (map-of (+ x v) (+ clk 2))]))

(defn state-seq [state instructions]
  (reduce
    #(into %1 (more-states (peek %1) %2))
    [state]
    instructions))

(defn sig-str [sseq n] (apply * (vals (nth sseq (dec n)))))

(defn draw-row [row]
  (apply str (map-indexed #(if (<= (dec %1) (:x %2) (inc %1)) "\uD83D\uDFE9" "⬛️") row)))

(defn p1 []
  (let [sig-str* (partial sig-str (state-seq init-state prog))]
    (transduce (map sig-str*) + [20 60 100 140 180 220])))

(defn p2 []
  (->> (state-seq init-state prog)
       (partition-all 40)
       (mapv (comp println draw-row))))
