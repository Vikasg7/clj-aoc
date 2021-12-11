(ns clj-aoc.2021.02.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn steps [[f d] step]
  (match (vec step)
    ["forward" x] [(+ f x) d]
    ["down"    x] [f       (+ d x)]
    ["up"      x] [f       (- d x)]))

(defn solve [input]
  (->> input
       (reduce steps [0 0])
       (multiply)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"\s+")
       (mapv read-str)
       (partition 2)))

(defn -main [input-file]
  (solve (prepare input-file)))

(-main "src/clj_aoc/2021/02/input.txt")
