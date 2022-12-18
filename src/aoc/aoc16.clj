(ns aoc.aoc16
  (:require [aoc.colls :as c]
            [aoc.arrays :as a]
            [clojure.string :as str]
            [ubergraph.core :as u]
            [loom.alg :refer [all-pairs-shortest-paths]]
            [memento.core :as mem]
            [memento.config :as mem.c]))

(defn parse-input [in]
  (for [l (str/split-lines in)]
    (let [[_ id flow edges] (re-find #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)" l)]
      {:id (symbol id) :flow (parse-long flow) :edges (read-string (str "[" edges "]"))})))

(def test-input (parse-input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc16.txt")))

(defn shortest-paths [in]
  (let [g (apply u/graph (mapcat (fn [{:keys [id edges flow]}] (cons [id {:flow flow}] (map vector (repeat id) edges))) in))]
    (a/resolve-paths (all-pairs-shortest-paths g))))

(defn nodes-with-flow [in]
  (into {} (keep #(when (pos-int? (:flow %)) [(:id %) (:flow %)]) in)))

(defn search-max-flow
  [start t flow-nodes sp]
  (apply max 0
         (for [[n flow] flow-nodes]
           (let [time-at-dest (dec (- t (sp [start n])))]
             (if (pos-int? time-at-dest)
               (+ (* flow time-at-dest)
                  (search-max-flow n time-at-dest (dissoc flow-nodes n) sp))
               0)))))

(mem/memo #'search-max-flow {mem.c/type mem.c/caffeine mem.c/key-fn (partial take 3)})

(defn p1 [in] (search-max-flow 'AA 30 (nodes-with-flow in) (shortest-paths in)))

(defn p2 [in]
  (let [sp (shortest-paths in)
        flow-nodes (nodes-with-flow in)]
    (apply max
           (pmap
             (fn [[nodes1 nodes2]]
               (+ (search-max-flow 'AA 26 (select-keys flow-nodes nodes1) sp)
                  (search-max-flow 'AA 26 (select-keys flow-nodes nodes2) sp)))
             ;; try all splits of valves into 2 subsets, one is elephant and one is person
             (c/splits (keys flow-nodes))))))
