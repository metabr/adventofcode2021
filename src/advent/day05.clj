(ns advent.day05
  (:require
   [clojure.string :as string]))

(defn read-input []
  (let [lines (string/split (slurp "inputs/day05") #"\n")
        pairs (map #(string/split % #" -> ") lines)
        parse-pair (fn [pair] (map #(string/split % #",") pair))
        string-pairs (map parse-pair pairs)
        string->int (fn [[[x1 y1] [x2 y2]]] [[(Integer/parseInt x1)
                                              (Integer/parseInt y1)]
                                             [(Integer/parseInt x2)
                                              (Integer/parseInt y2)]])]
    (map string->int string-pairs)))

(defn coord-range [coords]
  (let [from (apply min coords)
        to   (inc (apply max coords))]
    (range from to)))

(defn line-points-no-diagonals [[[x1 y1] [x2 y2]]]
  (cond
    (== x1 x2)
    (map (fn [c] (vector x1 c)) (coord-range [y1 y2]))
    (== y1 y2)
    (map (fn [c] (vector c y1)) (coord-range [x1 x2]))))

(defn directed-range [a b]
  (cond
    (< a b)
    (range a (inc b))
    (> a b)
    (reverse (range b (inc a)))
    :else
    (repeat a)))

(defn line-points [[[x1 y1] [x2 y2]]]
  (cond
    (== x1 x2)
    (map (fn [c] (vector x1 c)) (coord-range [y1 y2]))
    (== y1 y2)
    (map (fn [c] (vector c y1)) (coord-range [x1 x2]))
    :else
    (->> (interleave (directed-range x1 x2)
                     (directed-range y1 y2))
         (partition 2)
         (map #(apply vector %)))))

(defn draw-diagram [freqs]
  (let [xs (map first (keys freqs))
        ys (map second (keys freqs))
        width (inc (apply max xs))
        height (inc (apply max ys))
        diagram (for [x (range width)
                      y (range height)]
                  (get freqs [x y] "."))]
    (doall
     (map println (apply map str (partition height diagram)))))
  freqs)

(defn run [& args]
  (let [input (read-input)]
    (->> (keep line-points-no-diagonals input)
         (apply concat)
         frequencies
         ;; draw-diagram
         (filter #(> (second %) 1))
         count
         (println "Number of points where at least two lines overlap:"))

    (->> (keep line-points input)
         (apply concat)
         frequencies
         ;; draw-diagram
         (filter #(> (val %) 1))
         count
         (println "Number of points where at least two lines (including diagonals) overlap:"))))
