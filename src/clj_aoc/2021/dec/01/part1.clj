(ns clj-aoc.2021.dec.01.part1
  (:use [clj-aoc.utils])
  (:require [clojure.string :as s :refer [split]]
            [clojure.core.match :refer [match]]))

(defn prepare [input-file]
  (->> (slurp input-file)
       (flip s/split #"\s")
       (mapv read-str)))

(defn -main [& args]
  )
