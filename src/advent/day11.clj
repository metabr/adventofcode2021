(ns advent.day11
  (:require
   [clojure.string :as string]))

(def all (for [x (range 10) y (range 10)] [x y]))

(defn neighbors [[x y]]
  (let [ns [[(dec x) y] [x (dec y)]
            [(inc x) y] [x (inc y)]
            [(dec x) (dec y)] [(inc x) (inc y)]
            [(inc x) (dec y)] [(dec x) (inc y)]]]
    (->> (filter (fn [[x y]] (and (>= x 0) (>= y 0))) ns)
         (filter (fn [[x y]] (and (< x 10) (< y 10)))))))

(defn increase-energy
  ([octopuses] (increase-energy octopuses all))
  ([octopuses coords]
   (reduce
    (fn [octopuses [x y]]
      (update-in octopuses [x y :energy] inc))
    octopuses
    coords)))

(defn to-flash [octopuses]
  (for [x (range 10)
        y (range 10)
        :let [{:keys [energy flashed]} (get-in octopuses [x y])]
        :when (and (> energy 9)
                   (not flashed))]
    [x y]))

(defn flash [octopuses coords-to-flash]
  (let [coords-to-increase (->> (map neighbors coords-to-flash)
                                (apply concat))
        octopuses (reduce
                   (fn [octopuses [x y]]
                     (assoc-in octopuses [x y :flashed] true))
                   octopuses
                   coords-to-flash)]
        (increase-energy octopuses coords-to-increase)))

(defn mapv-2d [f m]
  (mapv (fn [row] (mapv f row)) m))

(defn step [[octopuses flashed]]
  (loop [octopuses (increase-energy octopuses)
         coords-to-flash (to-flash octopuses)
         flashed (count coords-to-flash)]
    (if (empty? coords-to-flash)
      [(mapv-2d (fn [{:keys [flashed energy] :as val}]
                  (if flashed
                    {:energy 0 :flashed false}
                    val))
                octopuses)
       flashed]
      (let [octopuses (flash octopuses coords-to-flash)
            coords-to-flash (to-flash octopuses)]
        (recur octopuses coords-to-flash (+ flashed (count coords-to-flash)))))))

(defn run [& _]
  (let [input (->> (string/split (slurp "inputs/day11") #"\n")
                   (mapv-2d #(hash-map :energy (Integer/parseInt (str %))
                                       :flashed false)))]
    (->> (take 101 (iterate step [input 0]))
         (map second)
         (reduce +)
         (println "Total number of flashes:"))
    (->> (iterate step [input 0])
         (map-indexed (fn [idx [_ flashes]] [idx flashes]))
         (filter (fn [[idx flashes]] (== flashes 100)))
         first
         first
         (println "First synchronized flash happens at step"))))
