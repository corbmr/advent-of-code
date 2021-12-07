(ns corbmr.aoc.2021.3
  (:use [clojure.string :only [split-lines]]))

(defn input [file]
  (->> file
       (slurp)
       (split-lines)
       (map #(Integer/parseInt % 2))))

(defn to-num [bits]
  (->> bits
       (map-indexed (fn [i b] (if b (bit-shift-left 1 i) 0)))
       (reduce bit-or 0)))

(defn process [bits lines]
  (let [counts (reduce (fn [acc line]
                         (let [b (mapv (fn [i] (if (bit-test line i) 1 0)) (range bits))]
                           (mapv + acc b)))
                       (vec (repeat bits 0))
                       lines)
        length (/ (count lines) 2)
        gamma (mapv (fn [v] (> v length)) counts)
        epsilon (mapv not gamma)]
    (* (to-num gamma) (to-num epsilon))))

(defn oxygen [nums]
  (reductions #(process-2 %1 %2 true) nums (range 5))
  )

(defn process-2 [nums i branch]
  (let [groups (group-by #(bit-test % i) nums)
        c (compare (count (groups true)) (count (groups false)))]
    (cond
      (pos? c) (groups true)
      (neg? c) (groups false)
      :else (groups branch))
    ))

