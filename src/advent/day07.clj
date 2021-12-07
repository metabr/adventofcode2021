(ns advent.day07
  (:require
   [clojure.string :as string]))

(defn fuel [crabs pos]
  (let [crab-fuel (fn [crab] (Math/abs (- crab pos)))]
    (->>
     (map crab-fuel crabs)
     (reduce + 0))))

(defn fuel2 [crabs pos]
  (let [crab-distance (fn [crab] (Math/abs (- crab pos)))
        crab-fuel (fn [crab] (apply + (range (inc (crab-distance crab)))))]
    (->>
     (map crab-fuel crabs)
     (reduce + 0))))

(defn run [& args]
  (let [crabs (->> (string/split (slurp "inputs/day07") #",")
                   (map read-string))
        min-pos (apply min crabs)
        max-pos (apply max crabs)]
    (time
     (println "Part 1 [position fuel]:"
              (->> (range min-pos (inc max-pos))
                   (map-indexed (fn [i pos] [i (fuel crabs pos)]))
                   (sort-by second)
                   first)))
    (time
     (println "Part 2 [position fuel]:"
              (->> (range min-pos (inc max-pos))
                   (map-indexed (fn [i pos] [i (fuel2 crabs pos)]))
                   (sort-by second)
                   first)))))
