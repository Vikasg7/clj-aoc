(ns clj-aoc.2021.06.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

;; This solution is different from part1 to get around Heap overflaw

(defn freqs [input]
  (let [freqs (frequencies input)]
  (->> (range 0 9)
       (mapv #(get freqs % 0)))))

(defn simulate [[zs & rst :as freqs]]
  (-> (vec rst)
      (update 6 + zs)
      (conj zs)))

(defn solve [input]
  (->> (iterate simulate (freqs input))
       (take-nth 256)
       (second) (sum)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #",")
       (mapv read-str)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/06/input.txt"))
