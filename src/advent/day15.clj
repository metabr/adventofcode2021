(ns advent.day15
  (:require
   [clojure.string :as string]
   [clojure.data.priority-map :refer [priority-map]]))

(defn line->vector [l]
  (->> (map str l)
       (mapv #(Integer/parseInt %))))

(defn neighbors [cave size [x y]]
  (->> [[x (inc y)] [(inc x) y]
        [x (dec y)] [(dec x) y]]
       (filter
        (fn [[x y]]
          (and (< -1 x size) (< -1 y size))))
       (mapcat #(vector % (get cave %)))
       (apply hash-map)))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
  [start f]
  (loop [q (priority-map start 0)
         r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn big-cave [cave size]
  (let [size' (* 5 size)
        update-risk (fn [r] (if (== r 9) 1 (inc r)))]
    (->>
     (for [x (range size')
           y (range size')
           :let [[x-target y-target] [(mod x size) (mod y size)]
                 shift (+ (int (/ x size)) (int (/ y size)))
                 value (->> (get cave [x-target y-target])
                            (iterate update-risk)
                            (drop shift)
                            first)]]
       [[x y] value])
     (apply concat)
     (apply hash-map))))

(defn run [& _]
  (let [input (->> (slurp "inputs/day15")
                   string/split-lines
                   (mapv line->vector))
        size (count input)
        cave (->> (for [x (range size) y (range size)] [x y])
                  (mapcat (fn [pair] [pair (get-in input pair)]))
                  (apply hash-map))
        start [0 0]
        end [(- size 1) (- size 1)]
        size' (* 5 size)
        end' [(- size' 1) (- size' 1)]]
    (time
     (println "Lowest total risk:"
              (get (dijkstra start (partial neighbors cave size)) end)))
    (time
     (println "Lowest total risk (big cave):"
              (get (dijkstra start (partial neighbors (big-cave cave size) size')) end')))))
