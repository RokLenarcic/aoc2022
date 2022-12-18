(ns aoc.aoc18
  (:require
    [aoc.colls :as c]
    [clojure.string :as str]
    [aoc.arrays :as a]
    [loom.alg :as l.alg]
    [medley.core :as m]
    [ubergraph.core :as u]))

(defn parse-input [in]
  (into #{} (map #(read-string (str "[" % "]"))) (str/split-lines in)))

(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc18.txt")))
(def test-input (parse-input "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5"))

(def adjacent (partial a/adjacent* [[0 1 0] [1 0 0] [-1 0 0] [0 -1 0] [0 0 1] [0 0 -1]]))

(defn p1 [in] (->> (mapcat adjacent in) (remove in) count))

(defn points-outside
  "Returns points that are 'outside', so connected to 0,0,0"
  [g]
  (->> (l.alg/connected-components g)
       (m/find-first #(c/has? % [0 0 0]))
       (into #{})))

(defn make-graph
  "Makes graph out of empty space"
  [in]
  ;; must go from -1 to max + 1 (and range is exclusive on end), so we always have space around the rock
  (let [[[minx miny minz] [maxx maxy maxz]] (a/bounds in)
        dim-range (fn [min max] (range (dec min) (+ 2 max)))
        all-points (c/product (dim-range minx maxx) (dim-range miny maxy) (dim-range minz maxz))
        empty-space (set (remove in all-points))]
    (apply u/graph
           (for [p1 empty-space p2 (adjacent p1) :when (empty-space p2)]
             [p1 p2]))))

(defn p2 [in]
  (let [outside (points-outside (make-graph in))]
    (->> (mapcat adjacent in) (filter outside) count)))
