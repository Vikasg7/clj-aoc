(ns clj-aoc.2021.10.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]))

(def char->score 
  (zipmap ")]}>" [3 57 1197 25137]))

(def opening? (set "(<[{"))

(def open (zipmap ">])}" "<[({"))

(defn illegal-char 
  ([pending]
    (illegal-char [] pending))
  ([[c & cs :as checked] [p & ps :as pending]]
    (cond (empty? pending) nil
          (opening? p)     (recur (cons p checked) ps)
          (= c (open p))   (recur cs ps)
          :else            p)))

(defn solve [input]
  (->> (keep illegal-char input)
       (mapv char->score)
       (sum)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (s/split-lines)
       (mapv seq)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/10/sample.txt"))
(time (-main "src/clj_aoc/2021/10/input.txt"))
