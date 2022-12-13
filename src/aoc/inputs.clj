(ns aoc.inputs
  (:require [clojure.string :as str]
            [clojure.java.io :as jio]))

(defn parse-numbers
  ([l] (parse-numbers false l))
  ([allow-neg? l]
   (if allow-neg?
     (map parse-long (re-seq #"-?\d+" l))
     (map parse-long (re-seq #"\d+" l)))))

(defn mapped
  "If (f it) returns non-nil resp use that otherwise use the item unchanged, this is used to avoid bindings."
  [f it]
  (or (f it) it))

(defn blocks
  "Lines of text separated by a blank line. Returns seq o blocks"
  [s item-parse-fn]
  (->> (partition-by str/blank? (str/split-lines s))
       (keep #(when-not (str/blank? (first %))
                (map item-parse-fn %)))))

(defmacro reg-parse [l bind reg statement & more-clauses]
  `(if-let [~bind (re-find ~reg ~l)]
     ~statement
     ~(when (seq more-clauses)
        (apply list `reg-parse l more-clauses))))

(defn char-array2d
  [s item-xf]
  (mapv
    (fn [row]
      (mapv item-xf row))
    (str/split-lines s)))
