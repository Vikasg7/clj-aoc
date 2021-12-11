(ns clj-aoc.2021.01.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn solve [input]
  (->> (partition 3 1 input)
       (mapv sum)
       (partition 2 1)
       (filter #(apply < %))
       (count)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"\s+")
       (mapv read-str)))

(defn -main [input-file]
  (solve (prepare input-file)))

(-main "src/clj_aoc/2021/01/input.txt")
