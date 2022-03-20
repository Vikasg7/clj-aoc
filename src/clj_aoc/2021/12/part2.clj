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

(defn visited? [[cave & path]]
  (when (lower-case? (first cave))
    (let [visits  (->> (remove (comp upper-case? first) path)
                       (frequencies))]
    (or (= (visits cave) 2)
        (when ;; checking other small caves for twice
              (some #{2} (vals (dissoc visits cave)))
          (= (visits cave) 1))))))

(defn paths [cave-map [path :as open]]
  (when-not (empty? open)
    (let [cave     (first path)
          more     (->> (cave-map cave)
                        (map #(cons % path)))]
    (cond (= cave "end")  (lazy-seq (cons path (paths cave-map (rest open))))
          (visited? path) (recur cave-map (rest open))
          :else           (recur cave-map (concat more (rest open)))))))

(defn solve [input]
  (count (paths (cave-map input) [["start"]])))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"(-|\s+)")
       (partition 2)))

(defn -main [input-file]
  (solve (prepare input-file)))

(time (-main "src/clj_aoc/2021/12/sample.txt"))
(time (-main "src/clj_aoc/2021/12/input.txt"))
