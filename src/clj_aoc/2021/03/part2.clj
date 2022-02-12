(ns clj-aoc.2021.03.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn rate->dec [rate]
  (to-dec (apply str rate)))

(defn oxy-res [grp]
  (let [[os zs] (mapv grp [1 0])]
  (if (>= (count os) (count zs)) os zs)))

(defn co2-res [grp]
  (let [[os zs] (mapv grp [1 0])]
  (if (<= (count zs) (count os)) zs os)))

(defn bit-criteria [res-fn idx input]
  (->> (group-by #(nth % idx) input)
       (res-fn)))

(defn rate 
  ([res-fn input]
    (rate res-fn 0 input))
  ([res-fn idx input]
    (let [res (bit-criteria res-fn idx input)]
    (cond (one? (count res)) (first res)
          :else              (recur res-fn (inc idx) res)))))

(def oxy-rate (partial rate oxy-res))

(def co2-rate (partial rate co2-res))

(defn solve [input]
  (* (rate->dec (oxy-rate input))
     (rate->dec (co2-rate input))))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"\s+")
       (mapv read-strs)))

(defn -main [input-file]
  (solve (prepare input-file)))

(-main "src/clj_aoc/2021/03/input.txt")
