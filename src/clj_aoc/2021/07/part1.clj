(ns clj-aoc.2021.07.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

;; Use of type hint (^Integer) drastically improves the performance here.
(defn cost [input pos]
  (sum (mapv #(Math/abs ^Integer (- pos %)) input)))

(defn solve [input]
  (let [[mn mx] (minmax input)]
  (->> (a-range mn mx)
       (mapv (partial cost input))
       (minimum))))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #",")
       (mapv read-str)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/07/input.txt"))
