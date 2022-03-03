(ns clj-aoc.2021.13.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]))

(defn fold-paper 
  ([xs ys cmd]
    (match cmd
      [\x on] [(fold-paper on xs) ys]
      [\y on] [xs (fold-paper on ys)]))
  ([on dim]
    (map-if #(> % on) 
            #(- (* on 2) %) 
            dim)))

(defn solve [[[xs ys] cmds]]
  (->> (fold-paper xs ys (first cmds))
       (apply interleave)
       (partition 2)
       (into #{})
       (count)))

(defn prepare [input-file]
  (let [raw         (slurp input-file)
        [pnts cmds] (split raw #"\s{4}")
        pnts        (->> (split pnts #"(,|\s+)")
                         (mapv read-str)
                         (unravel 2))
        matcher     (re-matcher #"fold along (x|y)=(\d+)" cmds)
        cmds        (->> (repeatedly #(read-strs 
                                      (rest 
                                      (re-find matcher))))
                         (take-while (comp not empty?)))]
  [pnts cmds]))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/13/sample.txt"))
(time (-main "src/clj_aoc/2021/13/input.txt"))
