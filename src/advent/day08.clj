(ns advent.day08
  (:require
   [clojure.string :as string]))

(defn run [& _]
  (let [input (->> (string/split (slurp "inputs/day08") #"\n")
                   (map #(string/split % #" \| "))
                   (map (fn [[signals digits]] {:signal-pattern (string/split signals #" ")
                                                :output-digits (string/split digits #" ")})))
        is-1-4-7-or8? (fn [digit]
                        (let [len (count digit)]
                          (or (== len 2) ; this is 1
                              (== len 3) ; this is 7
                              (== len 4) ; this is 4
                              (== len 7) ; this is 8
                              )))]
    (->> input
         (map :output-digits)
         (apply concat)
         (filter is-1-4-7-or8?)
         count
         (println "1, 4, 7 or 8 appear this many times:"))))
