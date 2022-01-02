(ns clj-aoc.2021.09.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]))

(defn adjcents [board pos]
  (let [offsets [[0 0] [0 1] [1 0] [0 -1] [-1 0]]]
  (->> offsets
       (mapv #(add-vec pos %))
       (keep #(get-in board %)))))

(defn positions [board]
  (for [r (range 0 (count board))
        c (range 0 (count (first board)))]
    [r c]))

(defn lowest-point? [board position]
  (let [[cur & adjs] (adjcents board position)]
  (when (< cur (minimum adjs))
    cur)))

(defn risk-level [height]
  (inc height))

(defn solve [board]
  (->> (positions board)
       (keep #(lowest-point? board %))
       (mapv risk-level)
       (sum)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (s/split-lines)
       (mapv (comp read-strs seq))))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/09/input.txt"))
