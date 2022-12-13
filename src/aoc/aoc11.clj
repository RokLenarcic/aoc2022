(ns aoc.aoc11
  (:require [aoc.inputs :as inputs]))

(defn parse-monkey [l]
  (inputs/reg-parse l
    [_ id] #"Monkey (\d+):" [:id (parse-long id)]
    [_ items] #"Starting items: (.*)" [:items (read-string (str "[" items "]"))]
    [_ op oper] #"Operation: new = old (.*) old" [:expr #((resolve (symbol op)) % %)]
    [_ op oper] #"Operation: new = old (.*) (.*)" [:expr #((resolve (symbol op)) % (parse-long oper))]
    [_ div] #"Test: divisible by (.*)" [:test (parse-long div)]
    [_ id] #"   If true: throw to monkey (.*)" [true (parse-long id)]
    [_ id] #"   If false: throw to monkey (.*)" [false (parse-long id)]))

(defn parse-input [s] (mapv #(into {:inspections 0} %) (inputs/blocks s parse-monkey)))

(def input (parse-inpuwt (slurp "/Users/roklenarcic/aoc/aoc11.txt")))
(def test-input (parse-input "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"))

(defn monkey-turn [relief-fn monkeys id]
  (let [{:keys [items expr test] :as m} (monkeys id)
        updated-monkey (-> m (assoc :items []) (update :inspections + (count items)))]
    (reduce
      #(update-in %1 [(m (zero? (mod %2 test))) :items] conj %2)
      (assoc monkeys id updated-monkey)
      (mapv (comp relief-fn expr) items))))

(defn run-round [relief-fn monkeys]
  (reduce (partial relief-fn monkey-turn) monkeys (range (count monkeys))))

(defn monkey-business [monkeys rounds relief-fn]
  (->> (nth (iterate (partial run-round relief-fn) monkeys) rounds)
       (map :inspections)
       sort reverse (take 2) (apply *)))

(defn p1 [] (monkey-business input 20 #(quot % 3)))

(defn p2 []
  (let [mod-factor (reduce * (map :test (vals input)))]
    (monkey-business input 10000 #(mod % mod-factor))))
