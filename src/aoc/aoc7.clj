(ns aoc.aoc7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.inputs :as inputs]))

(def input (rest (line-seq (io/reader "/Users/roklenarcic/aoc/aoc7.txt"))))

(def test-input (rest (str/split "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
                                 #"\n")))

(defn parse [l]
  (inputs/reg-parse
    l
    _ #"^\Q$ cd ..\E" {:what :dir-up}
    [_ where] #"^\$ cd (.+)" {:what :dir-change :where where}
    [_ dir-name] #"^dir (.+)" {:what :obj :dir-name dir-name}
    [_ size file-name] #"^(\d+) (.+)" {:what :obj :file-name file-name :size (parse-long size)}))

(def parsed-input (keep parse input))
(def test-parsed-input (keep parse test-input))

(defn add-entry [tree path {:keys [file-name dir-name size]}]
  (if dir-name
    (update-in tree (conj path dir-name) #(or % {}))
    (assoc-in tree (conj path file-name) size)))

(defn construct-tree
  [commands]
  (:tree
    (reduce
      (fn [{:keys [path] :as acc} {:keys [what where] :as c}]
        (case what
          :obj (update acc :tree add-entry path c)
          :dir-up (update acc :path pop)
          :dir-change (update acc :path conj where)))
      {:tree {} :path []}
      commands)))

(def the-tree (construct-tree parsed-input))
(def the-test-tree (construct-tree test-parsed-input))

(defn dir-sizes [tree path]
  (reduce-kv
    (fn [acc obj-name v]
      (if (number? v)
        (update acc path (fnil + 0) v)
        (let [sub-path (str path "/" obj-name)
              sub-dir-sizes (dir-sizes v sub-path)]
          (update (merge acc sub-dir-sizes)
                  path (fnil + 0) (sub-dir-sizes sub-path)))))
    {}
    tree))

(defn p1 []
  (->> (dir-sizes the-tree "")
       vals
       (filter #(<= % 100000))
       (apply +)))

(defn p2 []
  (let [total-space 70000000
        required-space 30000000
        sizes (dir-sizes the-tree "")
        free-space (- total-space (sizes ""))]
    (->> (vals sizes)
         (filter #(>= % (- required-space free-space)))
         (apply min))))
