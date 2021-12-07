(ns corbmr.aoc.2021.6
  (:require [clojure.string :as str]))

(defn input [file]
  (mapv #(Integer/parseInt %)
        (-> file
            (slurp)
            (str/trim-newline)
            (str/split #","))))

(defn prepare [nums]
  (reduce (fn [arr [i v]] (assoc arr i v)) (vec (repeat 9 0)) (frequencies nums)))

(defn process-dumb [nums iters]
  (if (pos? iters)
    (process
     (mapcat (fn [n]
               (if (pos? n)
                 [(dec n)]
                 [6 8]))
             nums)
     (dec iters))
    nums))

(defn process-once [nums]
  (let [z (nums 0)]
    (dotimes [i 9]
      (assoc! nums i
              (condp = i
                6 (+ z (nums (inc i)))
                8 z
                (nums (inc i)))))
    nums))

(defn process [nums iters]
  (reduce + 0 (persistent! (nth (iterate process-once (transient nums)) iters))))
