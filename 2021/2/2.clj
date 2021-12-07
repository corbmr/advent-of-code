(ns corbmr.aoc.2021.2
  (:use [clojure.string :only [split-lines split]]))

(defn input [file]
  (->> file
       (slurp)
       (split-lines)
       (map (fn [l] (-> l
                        (split #" ")
                        (update 1 #(Integer/parseInt %)))))))

(defn process [lines]
  (let [{:keys [depth pos]}
        (reduce (fn [acc [command amount]]
                  (condp = command
                    "forward" (-> acc
                                  (update :pos + amount)
                                  (update :depth + (* amount (:aim acc))))
                    "up" (update acc :aim - amount)
                    "down" (update acc :aim + amount)))
                {:depth 0 :pos 0 :aim 0}
                lines)]
    (* depth pos)))
