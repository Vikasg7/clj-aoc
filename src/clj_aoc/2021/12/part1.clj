(ns clj-aoc.2021.12.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]))

(defn cave-map [input]
  (let [rev (mapv reverse input)]
  (->> (concat input rev)
       (group-by first)
       (flip update-vals (comp #(remove #{"start"} %)
                               #(mapv second %)))
       (flip dissoc "end"))))

(defn paths [[path :as open] cave-map]
  (when-not (empty? open)
    (let [cave     (first path)
          small?   (lower-case? (first cave))
          visited? (when small? (some #{cave} (rest path)))
          nopen    (->> (cave-map cave)
                        (map #(cons % path))
                        (concat (rest open)))]
    (cond (= cave "end") (lazy-seq (cons path (paths nopen cave-map)))
          visited?       (recur (rest open) cave-map)
          :else          (recur nopen cave-map)))))

(defn solve [input]
  (count (paths [["start"]] (cave-map input))))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"(-|\s+)")
       (partition 2)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/12/sample.txt"))
(time (-main "src/clj_aoc/2021/12/input.txt"))
