(ns clj-aoc.2021.03.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn gamma-rate [mid x]
  (if (> mid x) 1 0))

(defn elpsn-rate [mid x]
  (if (> mid x) 0 1))

(defn rate->dec [rate-fn frq]
  (->> (mapv rate-fn frq)
       (apply str)
       (to-dec)))

(defn solve [input]
  (let [cnt (count input)
        frq (reduce add-vec input)
        mid (/ cnt 2)]
  (* (rate->dec #(gamma-rate mid %) frq)
     (rate->dec #(elpsn-rate mid %) frq))))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"\s+")
       (mapv read-strs)))

(defn -main [input-file]
  (solve (prepare input-file)))

(-main "src/clj_aoc/2021/03/input.txt")
