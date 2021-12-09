(ns clj-aoc.2021.dec.02.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn steps [[a f d] step]
  (match (vec step)
    ["forward" x] [a       (+ f x) (+ d (* x a))]
    ["down"    x] [(+ a x) f       d]
    ["up"      x] [(- a x) f       d]))

(defn solve [input]
  (->> input
       (reduce steps [0 0 0])
       (rest)
       (multiply)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"\s+")
       (mapv read-str)
       (partition 2)))

(defn -main [input-file]
  (solve (prepare input-file)))

(-main "src/clj_aoc/2021/dec/02/input/part2.txt")
