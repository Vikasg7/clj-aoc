(ns clj-aoc.2021.05.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn straight? [[[x y] [a b]]]
  (or (= x a) (= y b)))

(defn straight-path [[[x y] [a b]]]
  (for [m (a-range x a)
        n (a-range y b)]
  [m n]))

(defn diagonal-path [[[x y] [a b]]]
  (let [m (a-range x a)
        n (a-range y b)]
  (mapv vector m n)))

(defn path [points]
  (cond (straight? points) (straight-path points)
        :else              (diagonal-path points)))

(defn solve [input]
  (->> input
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
