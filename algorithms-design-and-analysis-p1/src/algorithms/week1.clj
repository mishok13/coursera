(ns algorithms.week1
  "Inversion count

  http://en.wikipedia.org/wiki/Inversion_%28discrete_mathematics%29

  For an array [2 1 4 3 5] and inversion count is 2 and the inversions
  are: [2 1] and [4 3]. Likewise, for array [2 4 1 3 5] it's 3 and [2
  1], [4 1] and [4 3]."
  (:require
   [clojure.java.io :as io]))

(defn simple
  [coll]
  (->> coll
       (iterate rest)
       (take-while not-empty)
       (map #(count (filter (partial > (first %)) (rest %))))
       (reduce + 0)))

(defn combine
  [a b]
  (loop [r []
         a a
         b b]
    (cond
     (and (seq a) (seq b)) (if (< (first a) (first b))
                             (recur (conj r (first a)) (rest a) b)
                             (recur (conj r (first b)) a (rest b)))
     (seq a) (concat r a)
     (seq b) (concat r b)
     :else r)))

(defn split
  [coll]
  (let [middle (/ (count coll) 2)]
    [(take middle coll) (drop middle coll)]))

(defn merge-sort
  [coll]
  (case (count coll)
    (0 1) coll
    (apply combine (map merge-sort (split coll)))))

(defn combine-inversions
  [[ca a] [cb b]]
  (loop [c (+ ca cb)
         r []
         a a
         b b]
    (if (and (seq a) (seq b))
      (if (< (first a) (first b))
        (recur c (conj r (first a)) (rest a) b)
        (recur (+ c (count a)) (conj r (first b)) a (rest b)))
      [c (concat r a b)])))

(defn inversions-count*
  [coll]
  (case (count coll)
    (0 1) [0 coll]
    (apply combine-inversions (map inversions-count* (split coll)))))

(defn inversions-count
  [coll]
  (first (inversions-count* coll)))

(defn solve
  [path]
  (when path
    (let [data (-> path io/reader line-seq doall)]
      (println (inversions-count (map #(Long/parseLong %) data))))))
