(ns clj-aoc.2021.11.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]))

(defn adjcents [board pos]
  (let [offsets [[0 1] [0 -1] [1 0] [-1 0] [1 1] [1 -1] [-1 1] [-1 -1]]]
  (->> offsets
       (mapv #(add-vec pos %))
       (keep #(nth-in board %)))))

(defn map-cell [f board]
  (for [r (range 0 (count board))]
  (for [c (range 0 (count (first board)))]
    (f (nth-in board [r c])))))
      
(defn map-cell-indexed [f board]
  (for [r (range 0 (count board))]
  (for [c (range 0 (count (first board)))]
    (f [r c] (nth-in board [r c])))))

(defn flash
  ([board]
    (map-cell-indexed (partial flash board) board))
  ([board pos curr]
    (cond (zero? curr)  0 ;; already flashed
          (> curr 9)    0 ;; just flashed
          :else         (let [xtra (->> (adjcents board pos)
                                        (count-if #(> % 9)))]
                        (+ curr xtra)))))

(defn step [board]
  (->> (map-cell inc board)
       (fix-point flash)))

(defn sync? [board]
  (every? zero? (flatten board)))

(defn solve [board]
  (->> (iterate step board)
       (take-while (comp not sync?))
       (count)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (s/split-lines)
       (mapv (comp read-strs seq))))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/11/sample.txt"))
(time (-main "src/clj_aoc/2021/11/input.txt"))
