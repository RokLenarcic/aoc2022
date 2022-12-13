(ns aoc.aoc12
  (:require [aoc.arrays :as a]
            [aoc.inputs :as inputs]
            [ubergraph.core :as u]
            [ubergraph.alg :as alg]))

(def test-input (a/->ArrayKd (inputs/char-array2d "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi" identity)
                             a/dirs))
(def input (a/->ArrayKd (inputs/char-array2d (slurp "/Users/roklenarcic/aoc/aoc12.txt") identity)
                        a/dirs))

(defn edge-pred [g edge]
  (let [edge-val #(int (inputs/mapped {\E \z \S \a} (a/node-val g (% edge))))]
    (>= (inc (edge-val u/src)) (edge-val u/dest))))

(defn p1 [input]
  (let [g (a/array->graph input)
        start (first (a/find-val input \S))
        end (first (a/find-val input \E))]
    (-> (alg/shortest-path g {:start-node start :edge-filter (partial edge-pred g) :end-node end})
        alg/edges-in-path count)))

(defn p2 [input]
  (let [g (a/array->graph input)]
    (-> (alg/shortest-path g {:start-nodes (a/find-val input \a) :edge-filter (partial edge-pred g)})
        (alg/path-to (first (a/find-val input \E)))
        alg/edges-in-path count)))
