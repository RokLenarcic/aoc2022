(ns aoc.aoc15
  (:require [clojure.string :as str]
            [medley.core :as m]))

(defn man-dist [p1 p2] (apply + (mapv (comp abs -) p1 p2)))

(defn parse-term [t]
  (let [[_ sx sy bx by] (map parse-long (re-find #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" t))]
    {:pos [sx sy] :beacon [bx by] :dist (man-dist [sx sy] [bx by])}))

(defn parse-input [in] (->> in (str/split-lines) (map parse-term)))

(def test-input (parse-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc15.txt")))

(defn detected? [sensors p] (not (not-any? #(<= (man-dist (:pos %) p) (:dist %)) sensors)))

(defn p1 [in y]
  (let [x-coords (mapcat (fn [{[x _] :pos dist :dist}] [(+ x dist) (- x dist)]) in)]
    (count (filter (fn [x] (and (detected? in [x y]) (not-any? #(= [x y] (:beacon %)) in)))
                   (range (apply min x-coords) (inc (apply max x-coords)))))))

(defn diamond
  "Emits a diamond shape at size x."
  [[x y] size]
  (let [l1 (fn [i] [(+ x i) (+ (- y size) i)])
        l2 (fn [i] [(- (+ x size) i) (+ y i)])
        l3 (fn [i] [(- x i) (- (+ y size) i)])
        l4 (fn [i] [(+ (- x size) i) (- y i)])]
    (mapcat (juxt l1 l2 l3 l4) (range size))))

(defn outer-diamond-within [min-coord max-coord {:keys [pos dist]}]
  (filter #(and (<= min-coord (first %) max-coord) (<= min-coord (second %) max-coord)) (diamond pos (inc dist))))

(defn p2 [in]
  ;; since there's only 1 possible location it must be at diamond size of detection edge + 1 of every
  ;; sensor
  (let [candidates (mapcat (partial outer-diamond-within 0 4000000) in)]
    (m/find-first #(not (detected? in %)) candidates)))
