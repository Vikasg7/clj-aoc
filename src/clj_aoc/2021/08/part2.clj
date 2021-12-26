(ns clj-aoc.2021.08.part2
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split split-lines]]
            [clojure.core.match :refer [match]]
            [clojure.set :refer [intersection union difference]]))

(defn signal->segment [patterns]
  (let [P (group-by count patterns)
        [O F S E] (->> (mapcat P [2 4 3 7])
                       (mapv set)) ;; [1 4 7 8]
        a (difference S O)
        N (set (find-first #(one? (count (difference (set %) F a))) (P 6))) ;; 9
        g (difference N F a)
        e (difference E N)
        T (set (find-first #(one? (count (difference (set %) O a g))) (P 5))) ;; 3
        d (difference T O a g)
        b (difference F O d)
        V (set (find-first #(one? (count (difference (set %) a b d g))) (P 5))) ;; 5
        f (difference V a b d g)
        c (difference O f)]
  (zipmap (mapv first [a b c d e f g]) "abcdefg")))

(def segment->digit
  (zipmap (mapv seq ["abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"])
          (range 0 10)))

(def count->digit
  {2 1
   4 4
   3 7
   7 8})

(defn four-digit [signal->segment segment]
  (or (count->digit (count segment))
      (segment->digit (sort (mapv signal->segment segment)))))

(defn four-digits [[patterns segments]]
  (mapv (partial four-digit (signal->segment patterns)) segments))

(defn coll->number [coll]
  (read-str (apply str coll)))

(defn solve [input]
  (->> input
       (mapv (comp coll->number four-digits))
       (sum)))

(defn prepare [input-file]
  (->> (slurp input-file)
       (s/split-lines)
       (mapv #(->> (s/split % #" ")
                   (split-by #{"|"})))))

(prepare "src/clj_aoc/2021/08/sample.txt")

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/08/input.txt"))
