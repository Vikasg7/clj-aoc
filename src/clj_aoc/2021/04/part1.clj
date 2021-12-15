(ns clj-aoc.2021.04.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]]))

(defn pentas [board]
  (concat (partition 5 board)
          (mapv #(take-nth 5 (drop % board)) (range 5))))

(defn won? [pentas]
  (some empty? pentas))

(defn winning-board [boards]
  (find-first won? boards))

(defn mark [draw pentas]
  (mapv #(remove #{draw} %) pentas))

(defn bingo [boards draw]
  (let [boards (mapv #(mark draw %) boards)]
  (if-let [board-won (winning-board boards)]
    (reduced (* draw (/ (sum (flatten board-won)) 2)))
; (else
    boards)))

(defn solve [input]
  (let [[draws nums] (split-at 100 input) ;; (split-at 27 input) ;; for sample
        boards       (mapv pentas (partition 25 nums))]
  (reduce bingo boards draws)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"(\s+|,)")
       (mapv read-str)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/04/input.txt"))
