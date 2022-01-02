(ns clj-aoc.2021.09.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]))

(defn board->map [board]
  (into {}
  (for [r (range 0 (count board))
        c (range 0 (count (first board)))
        :let [pos [r c]
              val (get-in board pos)]
        :when (not= val 9)]
    [pos val])))

(defn adjcents [pos]
  (let [offsets [[0 1] [1 0] [0 -1] [-1 0]]]
  (->> offsets
       (mapv #(add-vec pos %)))))

(defn basins
  ([bm]
    (basins bm [] [] #{[0 0]} #{}))
  ([bm basin basins open clsd]
    (let [curr (first open)]
    (cond (empty? bm)   basins
          (empty? open) (let [bm   (remove-keys bm basin)
                              open #{(first (first bm))}]
                        (recur bm [] (cons basin basins) open clsd))
          :else         (let [clsd (into clsd [curr])
                              open (->> (adjcents curr)
                                        (filterv bm)
                                        (concat (rest open))
                                        (remove clsd)
                                        (into #{}))
                              basin (if (bm curr) (cons curr basin) basin)]
                        (recur bm basin basins open clsd))))))

(defn desc [x y]
  (compare y x))

(defn solve [board]
  (->> (basins (board->map board))
       (mapv count)
       (sort desc)
       (take 3)
       (multiply)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (s/split-lines)
       (mapv (comp read-strs seq))))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/09/sample.txt"))
(time (-main "src/clj_aoc/2021/09/input.txt"))
