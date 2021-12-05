(ns advent.day04
  (:require [clojure.string :as string]))

(defn parse-board [bs]
  (let [bs  (rest bs) ;; skip empty line
        bs' (drop 5 bs)
        b   (->> (take 5 bs)
                 (map clojure.string/triml)
                 (map #(clojure.string/split % #" +"))
                 (apply concat)
                 (map read-string)
                 (map #(vector % false))
                 (into []))]
    [b bs']))

(defn parse-boards [input]
  (loop [input (rest input)
         bs    []]
    (if (empty? input)
      bs
      (let [[b input'] (parse-board input)]
        (recur input' (conj bs b))))))

(def rows-and-cols
  (let [rows (partition 5 (range 25))
        cols (partition 5 (apply interleave rows))]
    (concat rows cols)))

(defn complete? [board cells]
  (->> (map #(get board %) cells)
       (map second)
       (filter (complement identity))
       empty?))

(defn wins? [board]
  ((complement empty?) (filter identity (map #(complete? board %) rows-and-cols))))

(defn mark-board [board number]
  (mapv (fn [[n mark]] (if (== n number) [n true] [n mark])) board))

(defn mark-boards [boards number]
  (mapv #(mark-board % number) boards))

(defn winning-board [boards]
  (first (filter wins? boards)))

(defn board-sum [board]
  (reduce + 0 (map first (filter #(not (second %)) board))))

(defn final-score [board n]
  (* (board-sum board) n))

(defn run [& args]
  (let [input (string/split (slurp "inputs/day04") #"\n")
        numbers (->> (string/split (first input) #",")
                     (map read-string))
        winning-boards
        (loop [boards (parse-boards input)
               numbers numbers
               number-called nil
               winning-boards []]
          (let [win (winning-board boards)]
            (if (empty? numbers)
              winning-boards
              (if (some? win)
                (recur (mark-boards (filter (complement wins?) boards) (first numbers))
                       (rest numbers)
                       (first numbers)
                       (conj winning-boards {:board win :after number-called :final-score (final-score win number-called)}))
                (recur (mark-boards boards (first numbers))
                       (rest numbers)
                       (first numbers)
                       winning-boards)))))]
    (println "First to win:" (first winning-boards))
    (println "Last to win:" (last winning-boards))))
