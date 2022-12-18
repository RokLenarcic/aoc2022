(ns aoc.arrays
  (:require
    [clojure.walk :as walk]
    [ubergraph.core :as u]))

(defn size* [rect-array]
  [(count rect-array) (count (first rect-array))])

(defn rot
  "Rotates coordinates 90 deg"
  [[row col] dir]
  (case dir
    ("R" :R) [col (* -1 row)]
    ("L" :L) [(* -1 col) row]))

(def dirs (vec (take 4 (iterate #(rot % :R) [0 1]))))
(def diagonal-dirs (vec (take 4 (iterate #(rot % :R) [1 1]))))

(defn p-sum
  "Sum two points"
  [p1 p2]
  (mapv + (or p1 [0 0]) (or p2 [0 0])))

(defn adjacent* [dirs p]
  (map (partial p-sum p) dirs))

(defn- rasterize-low
  "Uses Bresenham's alg"
  [x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        yi (Long/signum dy)
        dy (* yi dy)]
    (loop [x x1 y y1 d (- (* 2 dy) dx) acc []]
      (let [acc' (conj acc [x y])
            d' (+ d (* 2 (if (pos-int? d) (- dy dx) dy)))
            y' (cond-> y (pos-int? d) (+ yi))]
        (if (= x x2)
          acc'
          (recur (inc x) y' d' acc'))))))

(defn rasterize
  [[x1 y1] [x2 y2]]
  (if (< (abs (- y2 y1)) (abs (- x2 x1)))
    (if (> x1 x2)
      (vec (reverse (rasterize-low x2 y2 x1 y1)))
      (rasterize-low x1 y1 x2 y2))
    (if (> y1 y2)
      (vec (reverse (map (comp vec reverse) (rasterize-low y2 x2 y1 x1))))
      (map (comp vec reverse) (rasterize-low y1 x1 y2 x2)))))

(defn in-array? [arr coord]
  (some? (get-in arr coord))
  #_(if-some [row (get-in arr (butlast coord))]
    (contains? row (last coord))
    false))

(defn ray
  "Returns coords in a direction from a spot in kd array, includes origin"
  [rect-arr dir coord]
  (take-while (partial in-array? rect-arr) (iterate #(p-sum % dir) coord)))

(defn vray
  "Returns values in a direction from a spot in kd array, includes origin"
  [rect-arr dir coord]
  (map #(get-in rect-arr %) (ray rect-arr dir coord)))

(defn fan-out
  "Returns coords fanning out of a spot in kd array, returns seq of seq of coords, one subseq for one ray,
  does not include origin."
  [rect-arr dirs coord]
  (for [dir dirs] (ray rect-arr dir (p-sum dir coord))))

(defn border-coordinates [[rows cols]]
  (concat (mapcat #(list [0 %] [(dec rows) %]) (range cols))
          (mapcat #(list [% 0] [% (dec cols)]) (range 1 (dec rows)))))

(defn margin-coordinates
  "Coordinates just outside the rectangle"
  [[rows cols]]
  (concat (mapcat #(list [-1 %] [rows %]) (range -1 (inc cols)))
          (mapcat #(list [% -1] [% cols]) (range 0 rows))))

(defn rows-to-cols [arr]
  (apply mapv vector arr))

(defn find-coords [arr pred]
  (if (vector? arr)
    (->> (range (count arr))
         (mapcat (fn [idx]
                   (when-let [res (find-coords (arr idx) pred)]
                     (map (partial cons idx) res))))
         (remove nil?))
    (when (pred arr) '[()])))

(defprotocol IArrayKd
  (dim [this] "Array dimensions")
  (val-at [this p])
  (find-val [this v] "Returns a seq of points")
  (filter-arr [this pred] "Returns map of p->val that match (pred val)")
  (adjacent [this p] "Returns map of p->val of adjacent squares"))

(defprotocol IArrayMutKd
  (update-val [this p f])
  (set-val [this p v]))

(defrecord ArrayKd [arr dirs]
  IArrayKd
  (dim [this] (reduce #(conj %1 (count %2)) [] (take-while some? (iterate #(let [f (first %)] (when (vector? f) f)) arr))))
  (val-at [this p] (get-in arr p))
  (find-val [this v] (map vec (find-coords arr #(= % v))))
  (filter-arr [this pred] (let [coords (find-coords arr pred)]
                            (zipmap (map vec coords)
                                    (map #(val-at this %) coords))))
  (adjacent [this p] (let [coords (filter (partial in-array? arr) (map p-sum (repeat p) dirs))]
                       (zipmap coords (map #(val-at this %) coords))))
  IArrayMutKd
  (update-val [this p f] (->ArrayKd (update-in arr p f) dirs))
  (set-val [this p v] (->ArrayKd (assoc-in arr p v) dirs)))

(defn array->graph
  [arr]
  (let [nodes (filter-arr arr any?)
        ug-nodes (map (fn [[p v]] [p {:v v}]) nodes)
        ug-edges (for [n (keys nodes)
                       adj (adjacent arr n)]
                   [n (key adj)])]
    (apply u/digraph (concat ug-nodes ug-edges))))

(defn node-val [g node] (u/attr g node :v))

(defn bounds
  "Returns [[minx,miny], [maxx, maxy]]"
  [points]
  (reduce (fn [[mins maxes :as ret] p]
            (if ret
              [(mapv min p mins) (mapv max p maxes)]
              [p p]))
          nil
          points))

(defn resolve-paths
  "Some Loom algs return nested maps that represent a path {a {b {c 3},
  this returns map {[a c] 3}"
  [paths]
  (reduce-kv
    (fn [acc k v]
      (let [m (walk/postwalk
                #(if (map-entry? %)
                   (if (number? (val %)) % (val %))
                   %) v)]
        (merge acc (update-keys m #(vector k %)))))
    {}
    paths))
