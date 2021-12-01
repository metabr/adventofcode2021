(def input
  (->> (clojure.string/split (slurp "day01.input") #"\n")
       (map read-string)))

(defn times-increased [measurements]
  (loop [prev nil
         measurements measurements
         increased 0]
    (if (empty? measurements)
      increased
      (if (and prev (> (first measurements) prev))
        (recur (first measurements) (rest measurements) (inc increased))
        (recur (first measurements) (rest measurements) increased)))))

(defn sum [i] (reduce + 0 i))

(defn three-measurement-sums [measurements]
  (loop [input measurements
         output []]
    (let [three (take 3 input)]
      (if (> 3 (count three))
        output
        (recur (rest input) (conj output (sum three)))))))

(println "Number of measurements increased:" (times-increased input))
(println "Number of sums of a three-measurement sliding window increased:" (times-increased (three-measurement-sums input)))
