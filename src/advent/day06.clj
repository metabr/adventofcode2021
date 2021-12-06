(ns advent.day06
  (:require
   [clojure.string :as string]))

(defn day [freqs]
  (let [result
        {0 (get freqs 1 0)
         1 (get freqs 2 0)
         2 (get freqs 3 0)
         3 (get freqs 4 0)
         4 (get freqs 5 0)
         5 (get freqs 6 0)
         6 (+ (get freqs 7 0) (get freqs 0 0))
         7 (get freqs 8 0)
         8 (get freqs 0 0)}]
    ;; (println result)
    result))

(defn run [& args]
  (let [lanternfish (->> (string/split (slurp "inputs/day06") #",")
                         (map read-string)
                         frequencies)
        after-80-days (last (take 81 (iterate day lanternfish)))
        after-256-days (last (take 257 (iterate day lanternfish)))]
    (println (reduce + 0 (vals after-80-days)) "lanternfish after 80 days")
    (println (reduce + 0 (vals after-256-days)) "lanternfish after 256 days")))
