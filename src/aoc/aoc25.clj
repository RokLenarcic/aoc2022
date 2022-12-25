(ns aoc.aoc25
  (:require [aoc.colls :as c]
            [clojure.string :as str]))

(defn parse-num [s] (reduce #(+ (* 5 %1) ({\0 0 \1 1 \2 2 \- -1 \= -2} %2)) 0 s))

(defn write-num [n]
  (cond->> (["=" "-" "0" "1" "2"] (mod (+ n 2) 5))
    (< 2 n) (str (write-num (quot (+ n 2) 5)))))

(def test-input "1=-0-2\n12111\n2=0=\n21\n2=01\n111\n20012\n112\n1=-1=\n1-12\n12\n1=\n122\n")
(def input (slurp "/Users/roklenarcic/aoc/aoc25.txt"))

(defn p1 [in] (write-num (c/sum-of parse-num (str/split-lines in))))
