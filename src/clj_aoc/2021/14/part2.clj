(ns clj-aoc.2021.14.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]))

(defn step [rules [k v]]
  (-> (rules k)
      (update-vals #(* % v))))

(defn process [rules tmplt]
  (->> (map (partial step rules) tmplt)
       (apply (partial merge-with +))))

(defn ele-freqs [m]
  (let [eles  (flatten (keys m))
        freqs (interleave (vals m) (vals m))]
  (->> (map hash-map eles freqs)
       (apply merge-with +)
       (flip update-vals #(round (/ % 2))))))

(defn calc-result [freqs]
  (let [[min max] (minmax (vals freqs))]
  (- max min)))

(defn solve [[tmplt rules]]
  (-> (iterate (partial process rules) tmplt)
      (nth 40)
      (ele-freqs)
      (calc-result)))

(defn prepare [input-file]
  (let [raw             (slurp input-file)
        [tmplt & rules] (split raw #"( -> |\s+)")
        tmplt           (frequencies (pairs 2 tmplt))
        [ks vs]         (unravel 2 rules)
        rules           (->> (map s/join vs ks)
                             (map #(frequencies (pairs 2 %)))
                             (zipmap (map seq ks)))]
  [tmplt rules]))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/14/sample.txt"))
(time (-main "src/clj_aoc/2021/14/input.txt"))
