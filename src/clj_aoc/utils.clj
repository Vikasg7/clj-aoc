(ns clj-aoc.utils
  (:require [clojure.pprint :refer [pprint]]))

(defn debug [v]
  (println v)
  (identity v))

(defn flip
  ([f]
    (partial flip f))
  ([f & ls]
    (apply f (reverse ls))))

(def _ nil)

(defmacro defn-memo [n & body]
  `(def ~n (memoize ~(cons 'fn body))))

(defn sum [as]
  (reduce + as))

(defn sum= [y xs]
  (= y (sum xs)))

(defn multiply [as]
  (reduce * as))

(defn to-int [n]
  (try (Long/parseLong (str n))
  (catch Exception e false)))

(defn read-str [v]
  (if-let [i (to-int v)] (identity i)
  (if (= 1 (count v))    (first v)
                         (identity v))))

(defn read-strs [xs]
  (mapv read-str xs))

(defn pairs [[fst & rst]]
    (for [r rst] [fst r]))

(defn combos-of
  "Way faster than math.combinatorics/combinations"
  ([size xs]
    (->> (iterate rest xs)
         (take (inc (- (count xs) size)))
         (mapcat (partial combos-of (dec size) _))))
  ([size _ [fst & rst :as xs]]
    (cond (= 1 size) (pairs xs)
          :else      (->> (combos-of size rst)
                          (mapv (partial cons fst))))))

(defn in-range? [mn mx x]
  (and (>= x mn) (<= x mx)))

(def add-vec (partial mapv +))

(defn pair-mod [a b]
  (mapv mod a b))

(defn to-dec [str]
  (Integer/parseInt str 2))

(defn find-first [pred coll]
  (first (drop-while (complement pred) coll)))

(defn truthy? [val]
  (if val true false))

(defn separate-by [pred coll]
  (let [mp (group-by (comp truthy? pred) coll)]
  [(mp true) (mp false)]))
