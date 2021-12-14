(ns advent.day13
  (:require
   [clojure.string :as string]))

(defn fold [dots {:keys [along line]}]
  (let [sort-fn (if (= along "x")
                  first
                  second)
        dots (sort-by sort-fn dots)
        [half half'] (split-with #((partial > line) (sort-fn %)) dots)]
            (->>
             (map (fn [[x y]] (if (= along "x")
                                [(- (* 2 line) x) y]
                                [x (- (* 2 line) y)]))
                  half')
             (concat half)
             distinct)))

(defn dimensions [dots]
  [(inc (apply max (map first dots)))
   (inc (apply max (map second dots)))])

(defn print-paper [dots]
  (let [[x-size y-size] (dimensions dots)
        paper (into [] (repeatedly y-size #(into [] (repeat x-size "."))))]
    (->> dots
         (reduce (fn [paper [x y]]
                   (assoc-in paper [y x] "#"))
                 paper)
         (map println)
         doall)))

(defn run [& _]
  (let [input (string/split-lines (slurp "inputs/day13"))
        [dots folds] (split-with #(not (= % "")) input)
        folds (->> (rest folds) ;; drop "" left over after split-with
                   (map #(string/split % #"="))
                   (map #(hash-map :along (str (last (first %)))
                                   :line (Integer/parseInt (second %)))))
        dots (->> dots
                  (map #(string/split % #","))
                  (mapv #(vector (Integer/parseInt (first %))
                                 (Integer/parseInt (second %)))))]

    (->> (first folds)
         (fold dots)
         count
         (println "Points visible after first fold:"))

    (doall
     (->>
      (reduce (fn [dots f]
                (fold dots f))
              (fold dots (first folds))
              (rest folds))
      print-paper))))
