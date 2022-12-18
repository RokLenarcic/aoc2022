(ns aoc.aoc19
  "This was one tricky, but I'll write this up , mainly because I
  gave up too quickly on my ideas of pruning the tree and went for cache + microoptimization, which proved insufficient.

  Caching speeds things up, but there's not enough duplication to really make a big difference. Tree pruning makes a
  much bigger difference. Try to find any sort of bound at all, even if expensive to compute it pays off, and sometimes
  something that seems minor can have major impact. Two prong approach is to extract bounds from the process and
  also to look at the quantities and look for bounds there. Here we used:
  - cache in form of unsynchronized HashSet for speed
  - used vectors instead of maps
  - since only 1 robot can be made per turn, it is pointless to have so many robots you make more than
  the resources needed for any one robot each turn, e.g. no robot type needs more than 4 ore, so why have more
  than 4 ore robots... that provides upper bound on the number of each robot type.
  - tried calculating max possible score for a branch (current score + if we made a geode robot each turn), then
  skipping branches on the same node if still worse than best branch found on node. This was very weak, but when
  I compared to a global search maximum score, it proved powerful. I had to put a volatile into state for this global
  to work.
  - initially I would proceed turn by turn, making a geode robot is possible, else trying obsidian, clay, ore and
  doing nothing. Using an alternative approach where I choose a robot to make then jump ahead to where I have the
  resources (if possible at all) seemed like a silly idea, it's just a different way of same process, doesn't seem
  to prune anything, but it was the most powerful optimization of them all, it reduces the number of nodes so much by
  skipping all the wait branches. Also it greedily checks flows where more expensive robots are purchased first,
  when going step by step favored exploration of making cheap robots (since you cannot make an expesive one at most
  junctions)."
  (:require [aoc.inputs :as inputs]
            [clojure.string :as str])
  (:import (java.util HashSet)))

(def materials [:ore :clay :obsidian :geode])

(defn parse-cost [c]
  (let [get-typ #(or (some-> (re-find (re-pattern (str "(\\d+) " (name %))) c) second parse-long) 0)]
    (conj (mapv get-typ (butlast materials)) 0)))

(def geode 3)

(defn parse-input [in]
  (map #(inputs/reg-parse %
          [_ id ore-cost clay-cost obsidian-cost geode-cost]
          #"Blueprint (\d+):\s+Each ore robot costs ([^.]+)\.\s+Each clay robot costs ([^.]+)\.\s+Each obsidian robot costs ([^.]+)\.\s+Each geode robot costs ([^.]+)"
          {:time-left 0
           :ores (mapv (constantly 0) materials)
           :costs [(parse-cost ore-cost)
                   (parse-cost clay-cost)
                   (parse-cost obsidian-cost)
                   (parse-cost geode-cost)]
           :robots [1 0 0 0]})
       (str/split-lines in)))

(def test-input (parse-input "Blueprint 1:  Each ore robot costs 4 ore.  Each clay robot costs 2 ore.  Each obsidian robot costs 3 ore and 14 clay.  Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2:  Each ore robot costs 2 ore.  Each clay robot costs 3 ore.  Each obsidian robot costs 3 ore and 8 clay.  Each geode robot costs 3 ore and 12 obsidian"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc19.txt")))

(defn init-factory [blueprint time-left]
  (let [v (volatile! 0)]
    (assoc blueprint :time-left time-left :max-geode v :update-max (fn [x] (vreset! v (max @v x)))
                     ;; calculate max useful robots of each type, geode is statically 100
                     :maximums (assoc (apply mapv #(apply max %&) (-> blueprint :costs)) geode 100))))

(defn build
  "Build a robot, consuming materials"
  [factory typ]
  (-> (update factory :ores (partial mapv -) (get-in factory [:costs typ]))
      (update-in [:robots typ] inc)))

(defn turns-to-produce
  "Turns to get from start to/over target with step per-turn, returns Long/MAX_VALUE if impossible"
  [start target per-turn]
  (if (pos-int? (- target start))
    (if (pos-int? per-turn)
      (int (Math/ceil (/ (- target start) per-turn)))
      Long/MAX_VALUE)
    0))

(defn try-robot
  "Returns a state with robot built if available (e.g. passing turns will generate enough resources)"
  [{:keys [robots costs ores maximums] :as factory} typ]
  (let [;; wait for the resouce that takes the longest
        turns-move (apply max 0 (map turns-to-produce ores (costs typ) robots))]
    ;; the trick is that because we can only build one robot at the time, we only ever need as many resources in a
    ;; turn as the biggests costs for a robot are
    (when (and (not= (robots typ) (maximums typ))
               (not= turns-move Long/MAX_VALUE))
      (-> factory
          (update :ores #(mapv (fn [x1 x2] (+ x1 (* (inc turns-move) x2))) %1 %2) robots)
          (build typ)
          (update :time-left - (inc turns-move))))))

(defn upper-bound "What's the max score we can get from here" [factory]
  (+ (get-in factory [:ores geode])
     (apply + (take (:time-left factory) (iterate inc (get-in factory [:robots geode]))))))

(defn turn
  [{:keys [time-left update-max] :as factory} ^HashSet seen?]
  (if (pos-int? time-left)
    (when (.add seen? factory)
      (->> (range (count materials))
           (keep (partial try-robot factory))
           (mapv (fn [new-state]
                   (when (< @(:max-geode factory) (upper-bound new-state))
                     (turn new-state seen?))))))
    (update-max (+ (get-in factory [:ores geode])
                   ;; negative time-left leads to scaling the count back to 0 time-left
                   (* time-left (get-in factory [:robots geode]))))))

(defn p1 [in]
  (let [in* (map #(init-factory % 24) in)]
    (doall (pmap #(turn %1 (HashSet.)) in*))
    (apply + (map-indexed (fn [idx n] (* (inc idx) @(:max-geode n))) in*))))

(defn p2 [in]
  (let [in* (map #(init-factory % 32) (take 3 in))]
    (doall (pmap #(turn %1 (HashSet.)) in*))
    (apply * (map (comp deref :max-geode) in*))))
