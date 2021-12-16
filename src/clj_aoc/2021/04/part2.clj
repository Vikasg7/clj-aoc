(ns clj-aoc.2021.04.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn pentas [board]
  (concat (partition 5 board)
          (mapv #(take-nth 5 (drop % board)) (range 5))))

(defn won? [pentas]
  (some empty? pentas))

(defn mark [draw pentas]
  (mapv #(remove #{draw} %) pentas))

(defn calc-result [draw board]
  (* draw (/ (sum (flatten board)) 2)))

(defn bingo [[result boards] draw]
  (let [[won pending] (->> (mapv #(mark draw %) boards)
                           (separate-by won?))
        result (if-let [board-won (first won)]
                 (calc-result draw board-won)
             ; (else
                 result)]
    [result pending]))

(defn solve [input]
  (let [[draws nums] (split-at 100 input) ;; (split-at 27 input) ;; for sample
        boards       (mapv pentas (partition 25 nums))]
  (->> (reduce bingo [0 boards] draws)
       (first))))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"(\s+|,)")
       (mapv read-str)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/04/input.txt"))
