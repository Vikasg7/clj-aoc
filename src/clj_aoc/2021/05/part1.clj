(ns clj-aoc.2021.05.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn straight? [[[x y] [a b]]]
  (or (= x a) (= y b)))

(defn path [[[x y] [a b]]]
  (for [m (range (min x a) (inc (max x a)))
        n (range (min y b) (inc (max y b)))]
  [m n]))

(defn solve [input]
  (->> input
       (filterv straight?)
       (mapcat path)
       (frequencies) (vals)
       (filterv #(> % 1))
       (count)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"(,| -> |\s+)")
       (mapv read-str)
       (partition 2)
       (partition 2)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/05/input.txt"))
