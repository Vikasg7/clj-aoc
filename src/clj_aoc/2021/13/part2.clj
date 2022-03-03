(ns clj-aoc.2021.13.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split join split-lines]]
            [clojure.core.match :refer [match]]))

(defn fold-paper
  ([[xs ys cmds]]
    (reduce fold-paper [xs ys] cmds))
  ([[xs ys] cmd]
    (match cmd
      [\x on] [(fold-paper on \x xs) ys]
      [\y on] [xs (fold-paper on \y ys)]))
  ([on dim cords]
    (map-if #(> % on) 
            #(- (* on 2) %) 
            cords)))

(defn visualize
  ([pnts]
    (doall (map println
    (for [y (a-range 0 (maximum (mapv second pnts)))]
      (for [x (a-range 0 (maximum (mapv first pnts)))]
        (visualize pnts [x y]))))))
  ([pnts pnt]
    (cond (pnts pnt) "#"
          :else      " ")))

(defn solve [[[xs ys] cmds]]
  (->> (fold-paper [xs ys cmds])
       (apply interleave)
       (partition 2)
       (into #{})
       (visualize)))

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
