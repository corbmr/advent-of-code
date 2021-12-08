(ns corbmr.aoc.2021.7
  (:require [clojure.string :as str]))

(defn input [file]
  (mapv #(Integer/parseInt %)
        (-> (slurp file)
            (str/trim-newline)
            (str/split #","))))

(defn median [nums]
  (let [s (sort nums)]
    (nth s (/ (count s) 2))))

(defn process [nums]
  (let [m (median nums)]
    (->> nums
         (map #(Math/abs (- % m)))
         (reduce +))))

(defn steps [n]
  (/ (* n (inc n)) 2))

(defn diff [nums m]
  (->> nums
       (map #(steps (Math/abs (- % m))))
       (reduce +)))

(defn process-2 [nums]
  (->> (range (apply max nums))
       (map #(diff nums %))
       (apply min)))
