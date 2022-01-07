(ns clj-aoc.2021.10.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]))

(def char->score 
  (zipmap ")]}>" [1 2 3 4]))

(def opening? (set "(<[{"))

(def open (zipmap ">])}" "<[({"))

(def close (zipmap "<[({" ">])}"))

(defn closing-seq 
  ([pending]
    (closing-seq [] pending))
  ([[c & cs :as checked] [p & ps :as pending]]
    (cond (empty? pending) (mapv close checked)
          (opening? p)     (recur (cons p checked) ps)
          (= c (open p))   (recur cs ps)
          :else            nil)))

(defn score [closing-seq]
  (->> (mapv char->score closing-seq)
       (reduce #(-> %1 (* 5) (+ %2)))))

(defn solve [input]
  (->> (keep closing-seq input)
       (mapv score)
       (median)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (s/split-lines)
       (mapv seq)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/10/sample.txt"))
(time (-main "src/clj_aoc/2021/10/input.txt"))
