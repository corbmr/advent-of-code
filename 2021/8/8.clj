(ns corbmr.aoc.2021.8
  (:require [clojure.string :as str])
  (:use [clojure.set :only [intersection difference map-invert]]))

(defn input [file]
  (->> (slurp file)
       (str/split-lines)
       (mapv (fn [line]
               (mapv #(str/split % #" ") (str/split line #" \| "))))))

(defn process-1 [lines]
  (->> lines
       (mapcat #(% 1))
       (filter (fn [in]
                 (let [s (count in)]
                   (or (= s 2)
                       (= s 4)
                       (= s 3)
                       (= s 7)))))
       (count)))

(defn intersections [code s]
  (count (intersection code s)))

(defn differences [code s]
  (count (difference code s)))

(defn handle [num-map logic codes]
  (reduce (fn [m s]
            (let [ss (set s)] (assoc m (logic num-map ss) ss)))
          num-map codes))

(defn logic-6 [num-map ss]
  (cond
    (= (intersections (num-map 1) ss) 1) 6
    (= (differences (num-map 4) ss) 1) 0
    :else 9))

(defn logic-5 [num-map ss]
  (cond
    (= (intersections (num-map 1) ss) 2) 3
    (= (intersections (num-map 4) ss) 3) 5
    :else 2))

(defn cipher [line]
  (let [counts (group-by count line)]
    (-> {1 (set (first (counts 2)))
         4 (set (first (counts 4)))
         7 (set (first (counts 3)))
         8 (set (first (counts 7)))}
        (handle logic-6 (counts 6))
        (handle logic-5 (counts 5))
        (map-invert))))

(defn decode [cipher nums]
  (->> nums
       (map (fn [n] (cipher (set n))))
       (reduce (fn [sum n] (+ (* 10 sum) n)) 0)))

(defn process-2 [lines]
  (->> lines
       (map (fn [[codes nums]]
              (decode (cipher codes) nums)))
       (reduce +)))

