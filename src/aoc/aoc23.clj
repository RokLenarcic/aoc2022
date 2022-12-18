(ns aoc.aoc23
  (:require
    [aoc.arrays :as a]
    [clojure.string :as str]
    [aoc.inputs :as i]
    [medley.core :as m]))

(defn parse-input [in]
  (->> (str/split-lines in)
       (map-indexed (fn [r-idx row] (keep-indexed #(when (= %2 \#) [r-idx %1]) row)))
       (apply concat)
       (into #{})))

(def test-input (i/map2d "....#..\n..###.#\n#...#.#\n.#...##\n#.###..\n##.#.##\n.#..#.." #(when (= % \#) \#)))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc23.txt")))

(def dirs (cycle [[-1 0] [-1 -1] [-1 1]                     ;N
                  [1 0] [1 -1] [1 1]                        ;S
                  [0 -1] [-1 -1] [1 -1]                     ;W
                  [0 1] [-1 1] [1 1]]))

(defn try-dir [elves p dir]
  (when (not-any? elves (a/adjacent* dir p))
    (a/p-sum p (first dir))))

(defn new-location [poss-dirs elves p]
  (or (and (some #(elves (a/p-sum p %)) poss-dirs)
           (some #(try-dir elves p %) (partition 3 poss-dirs)))
      p))

(defn round [{:keys [elves dirs]}]
  {:elves
   (reduce
     (fn [elves [new-p old-ps]]
       (if (= (count old-ps) 1)
         (conj elves new-p)
         (into elves old-ps)))
     #{}
     (group-by (partial new-location (take 12 dirs) elves) elves))
   :dirs (drop 15 dirs)})

(defn empty-tiles [elves]
  (let [[[minx miny] [maxx maxy]] (a/bounds elves)]
    (- (* (- (inc maxx) minx) (- (inc maxy) miny)) (count elves))))

(defn p1 [in] (empty-tiles (:elves (nth (iterate round {:elves in :dirs dirs}) 10))))

(defn p2 [in]
  (let [rounds (iterate round {:elves in :dirs dirs})
        outcomes (map (fn [idx r r*] (when (= (:elves r) (:elves r*)) idx))
                      (range) rounds (rest rounds))]
    (inc (m/find-first some? outcomes))))
