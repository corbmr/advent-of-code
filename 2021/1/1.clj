(ns corbmr.aoc.2021.1
  (:use [clojure.string :only [split-lines]]))

(defn input [f]
  (->> f
      (slurp)
      (split-lines)
      (map #(Integer/parseInt %))))

(defn process [lines]
  (let [triplets (map + lines (next lines) (nnext lines))
        pairs (map vector triplets (next triplets))]
    (->> pairs
         (filter (fn [[x y]] (< x y)))
         (count))))

