(ns advent.day03
  (:require
   [clojure.string :as string]))

(defn init-freqs [len]
  {"0" (vec (take len (repeat 0)))
   "1" (vec (take len (repeat 0)))})

(defn update-freq-in-pos [freqs pv]
  (update-in freqs pv inc))

(defn update-freqs [freqs n]
  (reduce update-freq-in-pos
          freqs
          (map-indexed (fn [pos val] [(str val) pos]) n)))

(defn calculate-freqs [ns]
  (reduce update-freqs (init-freqs (count (first ns))) ns))

(defn most-frequent-in-pos [freqs pos]
  (let [count-0 (get-in freqs ["0" pos])
        count-1 (get-in freqs ["1" pos])]
    (if (> count-0 count-1) 0 1)))

(defn least-frequent-in-pos [freqs pos]
  (if (== 1 (most-frequent-in-pos freqs pos)) 0 1))

(defn bin->int [rate]
  (Integer/parseInt (apply str rate) 2))

(defn calculate-rating [input bit-fn]
  (loop [vals input
         freqs (calculate-freqs vals)
         pos 0]
    (if (== 1 (count vals))
      vals
      (let [filter-fn #(== (bit-fn freqs pos) (read-string (str (get % pos))))
            vals (filter filter-fn vals)
            freqs (calculate-freqs vals)]
        (recur vals freqs (inc pos))))))

(defn run [& args]
  (let [input (string/split (slurp "inputs/day03") #"\n")
        len   (count (first input))
        freqs (calculate-freqs input)
        sigma-rate (bin->int (apply str (map #(most-frequent-in-pos freqs %) (range len))))
        gamma-rate (bin->int (apply str (map #(least-frequent-in-pos freqs %) (range len))))
        oxygen-generator-rating (bin->int (calculate-rating input most-frequent-in-pos))
        co2-scrubber-rating (bin->int (calculate-rating input least-frequent-in-pos))]
    (println "Gamma rate:" gamma-rate)
    (println "Sigma rate:" sigma-rate)
    (println "Power consumption:" (* gamma-rate sigma-rate))
    (println "Oxygen generator rating:" oxygen-generator-rating)
    (println "CO2 scrubber rating:" co2-scrubber-rating)
    (println "Life support rating:" (* oxygen-generator-rating co2-scrubber-rating))))
