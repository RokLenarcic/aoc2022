(ns aoc.colls
  (:require [clojure.walk :refer [walk]]
            [medley.core :refer [find-first]]))

(defn take-while2
  "Returns a transducer of successive items from coll while
  (pred acc item) returns logical true. pred must be free of side-effects.

  Return of pred is next acc.

  Returns a transducer when no collection is provided."
  {:added "1.0"
   :static true}
  [pred]
  (fn [rf]
    (let [acc (volatile! nil)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (if-let [next-acc (pred @acc input)]
           (do (vreset! acc next-acc)
               (rf result input))
           (reduced result)))))))

(defn- ->sym
  "Extract a symbol from the form.

  They symbol extracted will be:
  - the first symbol not in function name function call position.
  - the first keyword in function call position

  e.g. (->sym '(1 23 (inc {:a a}))) -> 'a'
       (->sym '(:x y)) -> 'x'
       (->sym '(inc y)) -> 'y'"
  [form]
  (walk ->sym
        #(if (coll? %) (find-first symbol? %) (when (symbol? %) %))
        (cond (and (list? form) (symbol? (first form))) (rest form)
              (and (list? form) (keyword? (first form))) (symbol (name (first form)))
              (map? form) (mapcat identity form)
              :else form)))

(defmacro map-of
  "Creates map with symbol names as keywords as keys and
   symbol values as values.

   Example: (map-of id name) => {:id id :name name}"
  [& syms]
  `(zipmap ~(vec (map (comp keyword ->sym) syms)) ~(vec syms)))
