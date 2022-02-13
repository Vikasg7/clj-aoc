(ns clj-aoc.2021.12.part2
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

(defn visited? [cave path]
  (let [visits  (->> (remove (comp upper-case? first) path)
                     (frequencies))
        ;; checking other small caves for twice
        twice?  (some #{2} (vals (dissoc visits cave)))]
  (or (= (visits cave) 2)
      (when twice? (= (visits cave) 1)))))

(defn paths [[path :as open] cave-map]
  (let [cave  (first path)
        nopen (->> (cave-map cave)
                   ;; expanding current path
                   (map #(cons % path))
                   ;; not using flip overflows stack,
                   ;; coz we need to process current path first
                   ;; before moving on to the next one
                   (flip concat (rest open)))] 
  (cond (empty? open)   nil
        (= cave "end")  (lazy-seq (cons path (paths nopen cave-map)))
        :else           (let [small?   (lower-case? (first cave))
                              visited? (when small? (visited? cave (rest path)))]
                        (cond visited? (recur (rest open) cave-map)
                              :else    (recur nopen cave-map))))))

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
