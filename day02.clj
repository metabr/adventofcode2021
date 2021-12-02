(defn process-line [l]
  (let [[direction step] (clojure.string/split l #" ")]
    [direction (read-string step)]))

(def input
  (->> (clojure.string/split (slurp "day02.input") #"\n")
       (map process-line)))

(defn calculate-position [instructions]
  (loop [horizontal 0
         depth 0
         instructions instructions]
    (if (empty? instructions)
      [horizontal depth]
      (let [[direction step] (first instructions)]
        (case direction
          "forward" (recur (+ horizontal step) depth (rest instructions))
          "down"    (recur horizontal (+ depth step) (rest instructions))
          "up"      (recur horizontal (- depth step) (rest instructions)))))))

(defn calculate-position-2 [instructions]
  (loop [horizontal 0
         depth 0
         aim 0
         instructions instructions]
    (if (empty? instructions)
      [horizontal depth]
      (let [[direction step] (first instructions)]
        (case direction
          "forward" (recur (+ horizontal step) (+ depth (* aim step)) aim (rest instructions))
          "down"    (recur horizontal depth (+ aim step) (rest instructions))
          "up"      (recur horizontal depth (- aim step) (rest instructions)))))))

(let [[final-forward final-depth] (calculate-position input)]
  (println "Part 1")
  (println "Final submarine position:" [final-forward final-depth])
  (println final-forward "*" final-depth "=" (* final-forward final-depth)))

(let [[final-forward final-depth] (calculate-position-2 input)]
  (println "Part 2")
  (println "Final submarine position:" [final-forward final-depth])
  (println final-forward "*" final-depth "=" (* final-forward final-depth)))
