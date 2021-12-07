(ns corbmr.aoc.2021.5
  (:require [clojure.string :as str]))

(defn input [f]
  (->> f
       (slurp)
       (str/split-lines)
       (map (fn [line]
              (->> (str/split line #" -> ")
                   (mapv (fn [p] (mapv #(Integer/parseInt %) (str/split p #",")))))))))

(defn inc-range [v1 v2]
  (if (< v1 v2)
    (range v1 (inc v2))
    (range v1 (dec v2) -1)))

(defn process [line-segments]
  (->> line-segments
       (mapcat (fn [[[x1 y1] [x2 y2] :as point]]
                 (cond
                   ; vertical line
                   (= x1 x2)
                   (map vector (repeat x1) (inc-range y1 y2))

                   ; horizontal line
                   (= y1 y2)
                   (map vector (inc-range x1 x2) (repeat y1))

                   ; diagonal
                   (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))
                   (map vector (inc-range x1 x2) (inc-range y1 y2))

                   ; invalid line
                   :else [])))
       (frequencies)
       (filter (fn [[k v]] (> v 1)))
       (count)))
