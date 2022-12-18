(ns aoc.aoc21
  (:require [aoc.inputs :as inputs]
            [clojure.string :as str]))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map
         (fn [l]
           (inputs/reg-parse l
             [_ id n] #"(\w+): (-?\d+)" [(symbol id) [0 (parse-long n) 0]]
             [_ id x op y] #"(\w+): (\w+) (.) (\w+)" [(symbol id) {:op (symbol op)
                                                                   :oper [(symbol x) (symbol y)]}])))
       (into {})))

(def test-input (parse-input "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32"))
(def input (parse-input (slurp "/Users/roklenarcic/aoc/aoc21.txt")))

(defn op-type [x] (if (zero? (first x)) (if (zero? (last x)) :n :inv-var) :var))

(defmulti do-op (fn [op x y] op))
(defn div [x y] (if (or (zero? y) (zero? x)) 0 (/ x y)))

(defmethod do-op '+ [_ x y] (mapv + x y))
(defmethod do-op '- [_ x y] (mapv - x y))
(defmethod do-op '* [_ x y]
  (if (= :n (op-type x))
    (mapv (partial * (second x)) y)
    (mapv (partial * (second y)) x)))
(defmethod do-op '/ [_ x y]
  (if (= :n (op-type x))
    (mapv (partial div (second x)) (reverse y))
    (mapv #(div % (second y)) x)))

(defn simplify [t nodes]
  (if (vector? t)
    t
    (let [opers (mapv #(simplify % nodes) (map nodes (:oper t)))]
      (do-op (:op t) (first opers) (second opers)))))

(defn p1 [in] (simplify (in 'root) in))

(defn solve [[x c inv-x]]
  (if (zero? inv-x)
    (* -1 (/ c x))
    (solve [c inv-x 0])))

(defn p2 [in]
  (let [in* (-> in (assoc 'humn [1 0 0]) (assoc-in ['root :op] '-))]
    (solve (simplify (in* 'root) in*))))
