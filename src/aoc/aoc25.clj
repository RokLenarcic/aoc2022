(ns aoc.aoc25
  (:require [aoc.colls :as c]
            [clojure.string :as str]))

(defn parse-num [s]
  (reduce (fn [v c]
            (case c
              \0 (* 5 v)
              \1 (+ 1 (* 5 v))
              \2 (+ 2 (* 5 v))
              \- (- (* 5 v) 1)
              \= (- (* 5 v) 2))) 0 s))

(defn write-num [n]
  (loop [s "" n* n]
    (if (zero? n*)                                          ;incorrect if n=0 -> ""
      s
      (recur
        (str ([\= \- \0 \1 \2] (mod (+ n* 2) 5)) s)
        (quot (+ n* 2) 5)))))

(def test-input "1=-0-2\n12111\n2=0=\n21\n2=01\n111\n20012\n112\n1=-1=\n1-12\n12\n1=\n122\n")
(def input (slurp "/Users/roklenarcic/aoc/aoc25.txt"))

(defn p1 [in]
  (write-num (c/sum-of parse-num (str/split-lines in))))
