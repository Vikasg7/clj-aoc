(ns clj-aoc.2021.14.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]))

(defn process [rules tmplt]
  (->> (pairs 2 tmplt)
       (mapcat rules)
       (zip tmplt)))

(defn calc-result [[min max]]
  (- max min))

(defn solve [[tmplt rules]]
  (-> (iterate (partial process rules) tmplt)
      (nth 10)
      (frequencies)
      (vals)
      (minmax)
      (calc-result)))

(defn prepare [input-file]
  (let [raw             (slurp input-file)
        [tmplt & rules] (split raw #"( -> |\s+)")
        tmplt           (seq tmplt)
        rules           (->> (map seq rules)
                             (unravel 2)
                             (apply zipmap))]
  [tmplt rules]))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/14/sample.txt"))
(time (-main "src/clj_aoc/2021/14/input.txt"))
