(ns clj-aoc.2021.15.part2
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

(defn cost [board dims pnt]
  (let [t1-cost (board (mapv mod pnt dims))
        cost    (sum (cons t1-cost (mapv quot pnt dims)))]
  (-> cost (- 1) (mod 9) (+ 1))))

(defn neighs-with-cost [cost dest [cur d]]
  (let [n  (neighs dest cur)
        di (mapv (comp #(+ d %) cost) n)]
  (zipmap n di)))

(defn dijkstra
  ([board t1-dims size]
    (let [src     [0 0]
          que     (priority-map src 0)
          tn-dims (mapv #(* % size) t1-dims)
          dest    (mapv dec tn-dims)
          cost    (partial cost board t1-dims)]
    (dijkstra cost dest que {})))
  ([cost dest que dist]
    (let [[cur d] (peek que)]
    (cond (empty? que)  (dist dest)
          (dist cur)    (recur cost dest (pop que) dist)
          :else         (let [que'  (->> (neighs-with-cost cost dest [cur d])
                                         (merge-with min (pop que)))
                              dist' (assoc dist cur d)]
                        (recur cost dest que' dist'))))))

(defn solve [input]
  (let [rows (count input)
        cols (count (first input))
        size 5]
  (dijkstra (make-board input) [rows cols] size)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (s/split-lines)
       (mapv (comp read-strs seq))))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/15/sample.txt"))
(time (-main "src/clj_aoc/2021/15/input.txt"))
