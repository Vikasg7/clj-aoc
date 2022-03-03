(ns clj-aoc.utils
  (:require [clojure.pprint :refer [pprint]]))

(defn debug [v]
  (pprint v)
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

(defn median [coll]
  (-> (sort coll)
      (nth (quot (count coll) 2))))

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

(defn take-until
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
    (when-let [s (seq coll)]
      (if (pred (first s))
        (lazy-seq (cons (first s) nil))
        (lazy-seq (cons (first s) (take-until pred (rest s)))))))

(defn a-range [start end]
  "an better alternative to range function that is inclusive of end
   and also works when end is less than start."
  (cond (< end start) (range start (dec end) -1)
        :else         (range start (inc end) 1)))

(defn count-if [pred hay]
  (count (filter pred hay)))

(defn minmax [coll]
  [(reduce min coll) (reduce max coll)])

(defn minimum [coll]
  (reduce min coll))

(defn maximum [coll]
  (reduce max coll))

(defn split-by
  "split but for collections"
  ([pred coll]
    (when (seq coll) 
      (split-by pred [] coll))) 
  ([pred acc [fst & rst :as coll]]
    (cond (empty? coll) (cons acc nil)
          (pred fst)    (lazy-seq (cons acc (split-by pred [] rst)))
          :else         (recur pred (conj acc fst) rst))))

(defn one? [val]
  (= val 1))

(defn remove-keys [mp ks]
  (reduce dissoc mp ks))

(defn safe-nth [coll idx]
  (try (nth coll idx)
  (catch Exception e nil)))

(defn nth-in [hay needles]
  (reduce safe-nth hay needles))

(defn fix-point [f x]
  (let [y (f x)]
  (if (= x y) x
    (recur f y))))

(defn map-kv [f mp]
  (let [step (fn [m k v] (assoc m k (f k v)))]
  (reduce-kv step (empty mp) mp)))

(defn upper-case? [^Character c]
  (Character/isUpperCase c))

(defn lower-case? [^Character c]
  (Character/isLowerCase c))

(defn unravel [n s]
  [(take-nth n s) (take-nth n (rest s))])

(defn map-if [pred f coll]
  (let [mapper (fn [x] (if (pred x) (f x) x))]
  (map mapper coll)))
