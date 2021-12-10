(ns advent.day10
  (:require
   [clojure.string :as string]))

(def open-closed
  {\{ \},
   \[ \],
   \< \>,
   \( \)})

(def closed-open
  {\} \{,
   \] \[,
   \> \<,
   \) \(})

(def price
  {\} 1197,
   \] 57,
   \> 25137,
   \) 3})

(defn check-line [line]
  (loop [line line
         balance '()]
    (if (empty? line)
      nil
      (let [expected (peek balance)
            actual (first line)]
        (if (contains? open-closed actual)
          ;; open bracket
          (recur (rest line) (conj balance actual))
          ;; closing bracket
          (if (= (get closed-open actual) expected)
            (recur (rest line) (pop balance))
            [expected actual]))))))

(defn get-completion [line]
  (loop [line line
         balance '()]
    (if (empty? line)
      (map open-closed balance)
      (let [expected (peek balance)
            actual (first line)]
        (if (contains? open-closed actual)
          (recur (rest line) (conj balance actual))
          (if (= (get closed-open actual) expected)
            (recur (rest line) (pop balance))
            nil))))))

(defn completion-score [completion]
  (let [price {\) 1, \] 2, \} 3, \> 4}
        score-fn (fn [score bracket] (+ (* score 5) (price bracket)))]
    (reduce score-fn 0 completion)))

(defn run [& _]
  (let [input (clojure.string/split (slurp "inputs/day10") #"\n")]
    (->> (keep check-line input)
         (map #(price (second %)))
         (reduce +)
         (println "Total syntax error score:"))
    (let [completion-scores
          (->> (keep get-completion input)
               (map completion-score)
               sort)
          len (count completion-scores)]
      (println "Completion middle score:"
               (->> completion-scores
                    (drop (int (/ len 2)))
                    first)))))
