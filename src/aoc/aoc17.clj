(ns aoc.aoc17
  (:require [aoc.arrays :as a]
            [aoc.colls :as c]
            [clojure.string :as str]
            [medley.core :as m]))

(def test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
(def input (str/trim (slurp "/Users/roklenarcic/aoc/aoc17.txt")))

(defn max-height [state] (apply max 0 (map first state)))

;; row + coll
(def shapes [[[0 0] [0 1] [0 2] [0 3]]
             [[0 1] [1 0] [1 1] [1 2] [2 1]]
             [[0 0] [0 1] [0 2] [1 2] [2 2]]
             [[0 0] [1 0] [2 0] [3 0]]
             [[0 0] [1 0] [0 1] [1 1]]])

(defn init-shape [state shape]
  (let [max-row (apply max 0 (map first state))]
    (mapv #(a/p-sum [(+ max-row 4) 2] %) shape)))

(defn valid-position? [state [row col :as p]] (and (pos-int? row) (>= col 0) (< col 7) (nil? (state p))))
(defn valid-position?* [state shape] (every? #(valid-position? state %) shape))

(defn init-state [moves]
  {:state #{}
   :shape nil
   :shape-q (cycle shapes)
   :moves (cycle (seq moves))})

(defn apply-move
  ([shape state move]
   (let [dir (case move \< [0 -1] \> [0 1])]
     (let [new-shape (mapv #(a/p-sum dir %) shape)]
       (if (valid-position?* state new-shape) new-shape shape))))
  ([{:keys [state moves] :as ctx}]
   (-> (update ctx :shape apply-move state (first moves))
       (update :moves next))))

(defn apply-drop
  ([state shape]
   (let [new-shape (mapv #(a/p-sum [-1 0] %) shape)]
     (when (valid-position?* state new-shape) new-shape)))
  ([{:keys [state shape] :as ctx}]
   (if-let [new-shape (apply-drop state shape)]
     (assoc ctx :shape new-shape)
     (assoc ctx :shape nil :state (into state shape)))))

(defn drop-piece [ctx]
  ;; load up a shape and drop it
  (->> (-> ctx
           (assoc :shape (init-shape (:state ctx) (first (:shape-q ctx))))
           (update :shape-q next))
       (iterate #(-> % apply-move apply-drop))
       (m/find-first (comp nil? :shape))))

(defn p1 [in]
  (let [ctx (init-state in)]
    (max-height (:state (nth (iterate drop-piece ctx) 2022)))))

(defn p2 [in]
  (let [ctx (init-state in)
        heights (map (comp max-height :state) (iterate drop-piece ctx))
        height-diff (map #(- %2 %1) heights (rest heights))
        {:keys [length prefix prefix-vals cycle-vals]} (c/find-cycle height-diff 10 40)]
    (let [n 1000000000000
          cycles (quot (- n prefix) length)
          last-bit (rem (- n prefix) length)]
      (+ (c/sum-of prefix-vals) (* cycles (c/sum-of cycle-vals)) (c/sum-of (take last-bit cycle-vals))))))
