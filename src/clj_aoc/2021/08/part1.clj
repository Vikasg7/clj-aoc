(ns clj-aoc.2021.08.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]))

(defn solve [input]
  (->> (mapcat second input)
       (filterv (comp #{2 4 3 7} count))
       (count)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (s/split-lines)
       (mapv #(->> (s/split % #" ")
                   (split-by #{"|"})))))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/08/input.txt"))
