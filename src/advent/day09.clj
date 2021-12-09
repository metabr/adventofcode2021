(ns advent.day09
  (:require
   [clojure.string :as string]
   [clojure.set :as set]))

(defn dimensions [points]
  [(count points)
   (count (first points))])

(defn neighbors [x y [width height]]
  (let [ns [[(dec x) y] [x (dec y)]
            [(inc x) y] [x (inc y)]]]
    (->> (filter (fn [[x y]] (and (>= x 0) (>= y 0))) ns)
         (filter (fn [[x y]] (and (< x width) (< y height)))))))

(defn basin-neighbors [x y points]
  (->> (neighbors x y (dimensions points))
       (map (fn [[x y]] [[x y] (get-in points [x y])]))
       (filter #((complement ==) 9 (second %)))
       (filter #(> (second %) (get-in points [x y])))))

(defn low-point? [x y points]
  (let [neighbor-values (->> (neighbors x y (dimensions points))
                             (map #(get-in points %))
                             sort
                             distinct)
        point-value (get-in points [x y])]
    (apply < (conj neighbor-values point-value))))

(defn basin-new-points [basin points]
  (set/difference
   (->> (map (fn [[x y]] (basin-neighbors x y points)) basin)
        (map #(map first %))
        (apply concat)
        set)
   basin))

(defn basin [[x y] points]
  (loop [basin #{[x y]}
         new-points (basin-new-points basin points)]
    (if (empty? new-points)
      basin
      (let [new-basin (set/union basin new-points)]
        (recur new-basin (basin-new-points new-basin points))))))

(defn run [& _]
  (let [input (->> (string/split (slurp "inputs/day09") #"\n")
                   (mapv (fn [s] (mapv #(Integer/parseInt (str %)) s))))
        [width height] (dimensions input)
        low-points (for [x (range width) y (range height)
                         :when (low-point? x y input)]
                     [x y])]
    (->>
     (map #(get-in input %) low-points)
     (map inc)
     (reduce + 0)
     (println "Sum of the risk levels of all low points:"))
    (->>
     (map #(basin % input) low-points)
     (map count)
     (sort (comp - compare))
     (take 3)
     (reduce *)
     (println "Three largest basin sizes multiplied:"))))
