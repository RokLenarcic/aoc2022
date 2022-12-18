(ns aoc.aoc24
  (:require
    [aoc.arrays :as a]
    [aoc.inputs :as i]
    [aoc.math :refer [lcm]])
  (:import (java.util HashSet LinkedList)))

(def test-input (i/map2d "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#" identity))
(def input (i/map2d (slurp "/Users/roklenarcic/aoc/aoc24.txt") identity))

(defn init-blizzards [in]
  (let [[_ [max-row max-col]] (a/bounds (keys in))
        lane (fn [[row col] c]
               (case c
                 \> (map #(vector row %) (range 1 max-col))
                 \< (map #(vector row %) (range (dec max-col) 0 -1))
                 \v (map #(vector % col) (range 1 max-row))
                 \^ (map #(vector % col) (range (dec max-row) 0 -1))
                 nil))]
    (->> (keep (fn [[p c]] (some->> (lane p c) (cycle) (drop-while #(not= p %)))) in)
         ;; rotate and take positions of each blizzard, pack it
         (apply map vector)
         (take (lcm (dec max-row) (dec max-col))))))

(defn empty-space [in]
  (let [[maxx maxy] (second (a/bounds (keys in)))]
    (-> (for [row (range 1 maxx)
              col (range 1 maxy)]
          [row col])
        set
        (conj [0 1] [maxx (dec maxy)]))))

(defn init [in]
  (let [blizzard-states (-> in init-blizzards)
        ;; fields of empty space for each turn
        es (empty-space in)
        fields (mapv #(apply disj es %) blizzard-states)]
    {:fields fields
     :p [0 1]
     :minutes 1
     :end (a/p-sum (second (a/bounds (keys in))) [0 -1])}))

(defn valid-moves [{:keys [p fields minutes]}]
  (let [field-set (fields (mod minutes (count fields)))]
    (filter field-set (into #{p} (a/adjacent* a/dirs p)))))

(defn next-state [state new-p]
  (-> state
      (assoc :p new-p)
      (update :minutes inc)))

(defn cache-key [state] [(:p state) (mod (:minutes state) (count (:fields state)))])

(defn bfs [state]
  (let [q (LinkedList.)
        seen? (HashSet.)]
    (loop [s state]
      (or (and (= (:p s) (:end s)) s)
          (do (->> (valid-moves s)
                   (map (partial next-state s))
                   (filter #(.add seen? (cache-key %)))
                   (mapv #(.add q %)))
              (recur (.poll q)))))))

(defn p1 [in] (dec (:minutes (bfs (init in)))))

(defn p2 [in]
  (let [to-goal (bfs (init in))
        back (bfs (assoc to-goal :end [0 1]))
        to-goal-again (bfs (assoc back :end (:end to-goal)))]
    (dec (:minutes to-goal-again))))
