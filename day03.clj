(def input (clojure.string/split (slurp "day03.input") #"\n"))

(def len (count (first input)))

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
  (reduce update-freqs (init-freqs len) ns))

(def freqs (calculate-freqs input))

(println "Counts of 0's in position:" (freqs "0"))
(println "Counts of 1's in position:" (freqs "1"))

(defn most-frequent-in-pos [freqs pos]
  (let [count-0 (get-in freqs ["0" pos])
        count-1 (get-in freqs ["1" pos])]
    (if (> count-0 count-1) 0 1)))

(defn least-frequent-in-pos [freqs pos]
  (if (== 1 (most-frequent-in-pos freqs pos)) 0 1))

(def sigma-rate
  (map #(most-frequent-in-pos freqs %) (range len)))

(def gamma-rate
  (map #(least-frequent-in-pos freqs %) (range len)))

(println "Gamma rate binary:" (apply str gamma-rate))
(println "Sigma rate binary:" (apply str sigma-rate))

(defn seq-to-int [rate]
  (Integer/parseInt (apply str rate) 2))

(let [gr (seq-to-int gamma-rate)
      sr (seq-to-int sigma-rate)]
  (println "Gamma rate:" gr)
  (println "Sigma rate:" sr)
  (println "Power consumption:" (* gr sr)))

;; Part 2
(println "Part 2")

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

(def oxygen-generator-rating
  (calculate-rating input most-frequent-in-pos))

(def co2-scrubber-rating
  (calculate-rating input least-frequent-in-pos))

(let [ogr (seq-to-int oxygen-generator-rating)
      csr (seq-to-int co2-scrubber-rating)]
  (println "Oxygen generator rating:" ogr)
  (println "CO2 scrubber rating:" csr)
  (println "Life support rating:" (* ogr csr)))
