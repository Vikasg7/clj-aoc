(ns clj-aoc.2021.05.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn straight? [[[x y] [a b]]]
  (or (= x a) (= y b)))

(defn straight-path [[[x y] [a b]]]
  (for [m (range (min x a) (inc (max x a)))
        n (range (min y b) (inc (max y b)))]
  [m n]))

(defn diagonal-path 
  ([[[x y :as f] [a b :as t]]]
    (cond (and (> a x) (> b y)) (diagonal-path f t [1 1])
          (and (< a x) (< b y)) (diagonal-path f t [-1 -1])
          (< a x)               (diagonal-path f t [-1 1])
          (< b y)               (diagonal-path f t [1 -1])
          :else                 (throw "Unreachable diagonal-path")))
  ([from-pos to-pos offset]
    (->> (iterate (partial add-vec offset) from-pos)
         (take-until (partial = to-pos)))))

(defn path [points]
  (cond (straight? points) (straight-path points)
        :else              (diagonal-path points)))

(defn solve [input]
  (->> input
       (mapcat path)
       (frequencies) (vals)
       (filterv #(> % 1))
       (count)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"(,| -> |\s+)")
       (mapv read-str)
       (partition 2)
       (partition 2)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/05/input.txt"))
