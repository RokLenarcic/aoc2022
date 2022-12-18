(ns aoc.aoc22
  (:require [clojure.string :as str]
            [aoc.arrays :as a]
            [aoc.inputs :as i]
            [clojure.set :as s]
            [aoc.colls :as c]))

(def dir-v {:> [0 1] :v [1 0] :< [0 -1] :A [-1 0]})
(def rot
  (let [r {:> :v :v :< :< :A :A :>}]
    {:R r :L (s/map-invert r)}))

(defn parse-input [in]
  (let [[map inst] (str/split in #"\n\n")]
    {:inst (mapv (some-fn parse-long keyword) (re-seq #"\d+|[A-Z]+" inst))
     :m (i/map2d map #(when (not= \space %) %))}))

(defn invert-dir [dir] (mapv #(* -1 %) dir))
(defn start-point [m]
  (first (keep #(when (m [0 %]) [0 %]) (range))))

(def test-input (parse-input "        ...#\n        .#..\n        #...\n        ....\n...#.......#\n........#...\n..#....#....\n..........#.\n        ...#....\n        .....#..\n        .#......\n        ......#.\n\n10R5L5R10L4R5L5"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc22.txt")))
(defn find-wrap [m p dir]
  [(last (take-while m (rest (iterate (partial a/p-sum (invert-dir (dir-v dir))) p))))
   dir])

(defn border-coord
  "The coordinate that's the exiting position on the border."
  [[row col] dir]
  (case dir (:v :A) col (:> :<) row))

(defn enter-q-from
  "Get into the quadrant from the direction with relative border coordinate"
  [dir [q-row q-col] border-coord qs-size]
  (case dir
    :v [(* qs-size q-row) (+ (* qs-size q-col) border-coord)]
    :A [(dec (* qs-size (inc q-row))) (+ (* qs-size q-col) border-coord)]
    :> [(+ (* qs-size q-row) border-coord) (* qs-size q-col) ]
    :< [(+ (* qs-size q-row) border-coord) (dec (* qs-size (inc q-col)))]))

(defn wrap-to-quadrant
  "Quadrants are squares the size of cube sides, in the input some are used some aren't.

  Takes orig quadrant of the point and dir such as [1, 0] quadrant and dir, finds local coordinate
  on the border of the orig quadrant, possibly inverts it, then maps it to a different quadrant,
  returning position and new dir"
  [{q-size :size :as qs} p dir]
  ;; determine original quadrant
  (let [original-q (mapv #(quot % q-size) p)
        {:keys [invert? next-q next-dir]} (get-in qs [original-q dir])
        ;; quadrant local coordinates for p
        local-border-coord (cond->> (mod (border-coord p dir) q-size)
                             ;; sometimes we need to invert the local coordinate
                             invert? (- (dec q-size)))]
    [(enter-q-from next-dir next-q local-border-coord q-size) next-dir]))

(def qs-input
  {:size 50
   [0 1] {:< {:next-q [2 0] :next-dir :> :invert? true}
          :A {:next-q [3 0] :next-dir :>}}
   [0 2] {:v {:next-q [1 1] :next-dir :<}
          :> {:next-q [2 1] :next-dir :< :invert? true}
          :A {:next-q [3 0] :next-dir :A}}
   [1 1] {:< {:next-q [2 0] :next-dir :v}
          :> {:next-q [0 2] :next-dir :A}}
   [2 0] {:< {:next-q [0 1] :next-dir :> :invert? true}
          :A {:next-q [1 1] :next-dir :>}}
   [2 1] {:v {:next-q [3 0] :next-dir :<}
          :> {:next-q [0 2] :next-dir :< :invert? true}}
   [3 0] {:< {:next-q [0 1] :next-dir :v}
          :> {:next-q [2 1] :next-dir :A}
          :v {:next-q [0 2] :next-dir :v}}})

(defn init [m] (assoc m :p (start-point m) :dir :>))

(defn move [wrap-fn {:keys [p dir] :as m}]
  (let [p* (a/p-sum p (dir-v dir))]
    (case (m p*)
      \. (assoc m :p p*)
      \# nil
      nil (let [[p** dir*] (wrap-fn p dir)]
            (when (= \. (m p**)) (assoc m :p p** :dir dir*))))))

(defn do-inst [wrap-fn m inst]
  (if (keyword? inst)
    (update m :dir #(get-in rot [inst %]))
    (last (take (inc inst) (c/iterate* (partial move wrap-fn) m)))))

(defn the-number [m]
  (let [{:keys [p dir]} m
        [row col] p]
    ;; there are 1 basis rows and cols in the actual count
    (+ (* (inc row) 1000)
       (* (inc col) 4)
       ({:> 0 :v 1 :< 2 :A 3} dir))))

(defn run-sequence [{:keys [inst m]} wrap-fn]
  (the-number (reduce (partial do-inst wrap-fn) (init m) inst)))

(defn p1 [in] (run-sequence in (partial find-wrap (:m in))))
(defn p2 [in] (run-sequence in (partial wrap-to-quadrant qs-input)))
