(ns clj-aoc.2021.15.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]
            [clojure.data.priority-map :refer [priority-map]]))

(defn make-board [input]
  (into (sorted-map)
  (for [r (range 0 (count input))
        c (range 0 (count (first input)))]
    [[r c] (nth-in input [r c])])))

(defn in-board? [[m n] [x y] [a b]]
  (and (>= a m) (>= b n) 
       (<= a x) (<= b y)))

(defn neighs [dest pnt]
  (->> [[0 1] [1 0] [0 -1] [-1 0]]
       (mapv (partial add-vec pnt))
       (filterv (partial in-board? [0 0] dest))))

(defn dijkstra
  ([board]
    (let [src  [0 0]
          dest (key (last board))
          que  (priority-map src 0)]
    (dijkstra board dest que {})))
  ([board dest que dist]
    (let [[cur d] (peek que)]
    (cond (empty? que)  (dist dest)
          (dist cur)    (recur board dest (pop que) dist)
          :else         (let [n     (neighs dest cur)
                              di    (mapv (comp #(+ d %) board) n)
                              ndi   (zipmap n di)
                              que'  (merge-with min (pop que) ndi)
                              dist' (assoc dist cur d)]
                        (recur board dest que' dist'))))))

(defn solve [input]
  (dijkstra (make-board input)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (s/split-lines)
       (mapv (comp read-strs seq))))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/15/sample.txt"))
(time (-main "src/clj_aoc/2021/15/input.txt"))
