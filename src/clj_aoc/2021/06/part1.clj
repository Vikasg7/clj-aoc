(ns clj-aoc.2021.06.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn next-timer [timer]
  (cond (zero? timer) 6
        :else         (dec timer)))

(defn simulate [fishes]
  (concat (mapv next-timer fishes)
          (repeat (count-if zero? fishes) 8)))

(defn solve [input]
  (->> (iterate simulate input)
       (take-nth 80)
       (second) (count)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #",")
       (mapv read-str)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/06/input.txt"))
